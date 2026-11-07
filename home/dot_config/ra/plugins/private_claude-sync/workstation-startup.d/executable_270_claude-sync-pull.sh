#!/bin/bash
# claude-sync plugin — boot pull, and the periodic pull the push loop re-runs.
#
# Pulls every workstation's namespace under gs://${BUCKET}/claude-sync/workstations/
# and merges into /home/user/.claude/. Idempotent. Bails fast when the
# plugin is disabled or the bucket env var is missing.
#
# All claude-sync data lives under the claude-sync/ prefix inside the
# bucket so other plugins can share the same bucket without collisions.
#
# Two modes (see docs/adr/0008-periodic-pull.md):
#   (no args)    boot pull: run by workstation-startup.d, detaches itself.
#   --periodic   run in the background by the push loop (271_*.sh) every
#                pull_interval_seconds, so sessions from other workstations
#                arrive without a reboot. Same merge as the boot pull except
#                .claude.json is not touched (Claude Code rewrites it
#                constantly and the local copy is authoritative).
# A lock keeps pulls from overlapping.
#
# Merge model (see docs/adr/0002-merge-based-sync.md):
#   * projects/, todos/, plans/, session-env/ — namespace-scoped, UUID-
#     named. Staged under /var/cache/ra-claude-sync, then copied into the
#     live tree with `cp -an` so newer local files win over the GCS copy.
#   * history.jsonl — an append-only union log; each namespace holds a
#     partial view. UNION-MERGED: local + every namespace, torn lines
#     dropped, deduped, sorted by timestamp. Never overwritten wholesale
#     (that was the session-loss bug), and never replaced if Claude Code
#     appended to it while the merge ran.
#   * .claude.json — local file is authoritative for machine-local state
#     (auth, onboarding, counters); only per-project `history` arrays are
#     unioned in from other namespaces. Foreign config is never imported.
set -euo pipefail

PERIODIC=0
[ "${1:-}" = "--periodic" ] && PERIODIC=1

# --- Detach so the boot pull never blocks workstation startup ----------------
# The cross-namespace pull can move hundreds of MB across dozens of GCS
# namespaces and take many minutes. Run inline, it blocks the
# /etc/workstation-startup.d/ sequence long enough that Cloud Workstations
# tears the container down mid-boot — sshd never stabilizes and `ra connect`
# fails with "the workstation does not have a server running on port 22".
# Re-exec ourselves once, detached into a new session via `setsid --fork`
# (same pattern as the push daemon in 271_*.sh), and return immediately so
# boot completes; the pull then runs to completion in the background.
# RA_CLAUDE_SYNC_PULL_DETACHED guards against infinite re-exec. A periodic
# pull is already a background child of the push loop, so it never detaches.
if [ "${PERIODIC}" -eq 0 ] && [ "${RA_CLAUDE_SYNC_PULL_DETACHED:-0}" != "1" ]; then
    export RA_CLAUDE_SYNC_PULL_DETACHED=1
    # Re-exec output is suppressed; capture a setsid failure so a non-running
    # boot pull is auditable in the boot journal instead of silently no-op'ing.
    if ! setsid --fork "$0" </dev/null >/dev/null 2>&1; then
        echo "[claude-sync-pull] WARNING: detached re-exec of '$0' failed;" \
             "boot pull skipped this boot" >&2
    fi
    exit 0
fi

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"
LOG_FILE="/var/log/ra-claude-sync.log"
# Last-known-good workstation id, cached on the persistent home disk so a
# transient metadata blip can't flip our identity. See resolve_ws_id.
WS_ID_CACHE="${USER_HOME}/.ra-claude-sync/ws-id"
# Download stage + pull lock. /var/cache resets with the container, so the
# stage never grows past one boot's worth of other workstations' files.
CACHE_DIR="/var/cache/ra-claude-sync"
STAGE="${CACHE_DIR}/stage"
LOCK_FILE="/run/ra-claude-sync.pull.lock"

# Tee both stdout and stderr through to the shared log file so a sync
# regression leaves an auditable trail beyond the boot journal. A periodic
# pull's stdout already is that log file (the push loop's), so it skips the
# tee rather than writing every line twice.
mkdir -p "$(dirname "${LOG_FILE}")"
if [ "${PERIODIC}" -eq 0 ]; then
    exec > >(tee -a "${LOG_FILE}") 2>&1
fi

log() { echo "[claude-sync-pull] $(date -u +%H:%M:%S) $*"; }

# Resolve this workstation's stable id. Metadata is authoritative; we
# cache the last-known-good value and prefer it over the `hostname`
# fallback so a momentary metadata outage cannot fork our namespace
# (the metadata name and `hostname` differ on Cloud Workstations:
# "workstations-<uuid>" vs e.g. "personal"). Identical logic lives in the
# push loop heredoc in 271_*.sh — keep the two in sync.
# Returns 0 if resolved from metadata/cache, 1 if it had to use hostname.
resolve_ws_id() {
    local id="" i
    for i in 1 2 3; do
        id="$(curl -fs -H 'Metadata-Flavor: Google' \
            'http://metadata.google.internal/computeMetadata/v1/instance/name' \
            2>>"${LOG_FILE}" || true)"
        [ -n "${id}" ] && break
        id=""
        sleep 1
    done
    if [ -n "${id}" ]; then
        mkdir -p "$(dirname "${WS_ID_CACHE}")" 2>/dev/null || true
        printf '%s\n' "${id}" > "${WS_ID_CACHE}" 2>/dev/null || true
        printf '%s' "${id}"
        return 0
    fi
    if [ -r "${WS_ID_CACHE}" ]; then
        local cached
        cached="$(head -n1 "${WS_ID_CACHE}" 2>/dev/null || true)"
        if [ -n "${cached}" ]; then
            printf '%s' "${cached}"
            return 0
        fi
    fi
    printf '%s' "$(hostname)"
    return 1
}

# Union-merge JSONL files into a destination, in place.
#   $1 = destination path ; $2.. = extra source files to fold in.
# Tolerates torn/invalid lines (fromjson? // empty), drops only exact
# duplicate objects (`unique` — never drops a record that merely shares a
# timestamp/sessionId with another), sorts by timestamp. The temp file is
# created in the destination's own directory so the write-back is a true
# atomic rename (mv across filesystems would be a non-atomic copy). Never
# replaces the destination with an empty result (guards a jq failure from
# wiping history).
#
# Claude Code appends to history.jsonl whenever you submit a prompt, and the
# periodic pull runs while it is active, so the write-back is guarded:
#   * merged == current → leave the file alone (no inode swap every interval);
#   * the replacement takes over the destination's owner and mode, so a file
#     Claude Code appends to never becomes root-owned;
#   * if the file's size/inode/mtime changed while we merged, discard the
#     result and merge again (3 tries), rather than drop the appended lines.
# A window of microseconds remains between that last check and the rename.
file_sig() { stat -c '%s:%i:%y' -- "$1" 2>/dev/null || echo absent; }

merge_jsonl_history() {
    local dest="$1"; shift
    local -a files
    local f tmp sig attempt
    for attempt in 1 2 3; do
        files=()
        [ -f "${dest}" ] && files+=("${dest}")
        for f in "$@"; do [ -f "${f}" ] && files+=("${f}"); done
        [ "${#files[@]}" -gt 0 ] || return 0
        sig="$(file_sig "${dest}")"
        tmp="$(mktemp "${dest}.merge.XXXXXX")"
        if cat "${files[@]}" 2>>"${LOG_FILE}" \
            | jq -R -c 'fromjson? // empty | select(type=="object")' 2>>"${LOG_FILE}" \
            | jq -s -c 'unique | sort_by(.timestamp // 0) | .[]' 2>>"${LOG_FILE}" > "${tmp}" \
            && [ -s "${tmp}" ]; then
            if cmp -s "${tmp}" "${dest}"; then
                rm -f "${tmp}"
                return 0
            fi
            if [ -f "${dest}" ]; then
                chown --reference="${dest}" "${tmp}" 2>/dev/null || true
                chmod --reference="${dest}" "${tmp}" 2>/dev/null || true
            fi
            if [ "$(file_sig "${dest}")" = "${sig}" ]; then
                mv -f "${tmp}" "${dest}"
                return 0
            fi
            log "history.jsonl changed during merge; retrying (attempt ${attempt}/3)"
        fi
        rm -f "${tmp}"
        # A jq failure or empty result leaves the destination untouched.
        [ "$(file_sig "${dest}")" = "${sig}" ] && return 0
    done
    log "WARN: history.jsonl kept changing during merge; leaving it for the next pull"
    return 0
}

# Merge .claude.json: keep the local file as the authoritative base and
# union in only per-project `history` arrays from remote namespaces.
#   $1 = local .claude.json (base) ; $2.. = remote claude.json files.
# Skips any source (including the base) that is not valid JSON. Atomic,
# validated write-back.
merge_claude_json() {
    local base="$1"; shift
    [ -f "${base}" ] || return 0
    jq -e . "${base}" >/dev/null 2>&1 || { log ".claude.json base invalid; leaving as-is"; return 0; }
    local -a rfiles=()
    local f
    for f in "$@"; do
        [ -f "${f}" ] || continue
        jq -e . "${f}" >/dev/null 2>&1 && rfiles+=("${f}")
    done
    [ "${#rfiles[@]}" -gt 0 ] || return 0
    # Temp in the destination's own directory → atomic rename on write-back.
    local tmp; tmp="$(mktemp "${base}.merge.XXXXXX")"
    if jq -s '
        # order-preserving unique (history entries have no timestamp to sort by)
        def dedup_keep_order: reduce .[] as $x ([]; if any(.[]; . == $x) then . else . + [$x] end);
        .[0] as $base
        | reduce .[1:][] as $r ($base;
            reduce (($r.projects // {}) | to_entries[]) as $e (.;
                (($e.value.history) // []) as $rh
                | if ($rh | length) > 0
                  then .projects[$e.key].history =
                       (((.projects[$e.key].history // []) + $rh) | dedup_keep_order)
                  else . end
            )
          )
    ' "${base}" "${rfiles[@]}" > "${tmp}" 2>>"${LOG_FILE}"; then
        if [ -s "${tmp}" ] && jq -e . "${tmp}" >/dev/null 2>&1; then
            mv -f "${tmp}" "${base}"
            return 0
        fi
    fi
    rm -f "${tmp}"
    return 0
}

[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_CLAUDE_SYNC_ENABLED:-false}" = "true" ] || exit 0
if [ -z "${RA_PLUGIN_CLAUDE_SYNC_BUCKET:-}" ]; then
    log "RA_PLUGIN_CLAUDE_SYNC_BUCKET unset; skipping"
    exit 0
fi

BUCKET="${RA_PLUGIN_CLAUDE_SYNC_BUCKET}"

# One pull at a time. A periodic pull that finds the boot pull (or its own
# previous run) still going skips this round rather than race it for the
# stage. If flock is somehow absent we run unlocked, as before.
mkdir -p "$(dirname "${LOCK_FILE}")" "${CACHE_DIR}"
exec 9>"${LOCK_FILE}"
if command -v flock >/dev/null 2>&1 && ! flock -n 9; then
    log "another pull is already running; skipping"
    exit 0
fi

# Namespace = this workstation instance's own id, never a shared one
# (docs/adr/0007-unique-workstation-identifiers.md). RA_PLUGIN_CLAUDE_SYNC_NAMESPACE
# — the per-config id older `ra create` runs inject — is deliberately ignored:
# every workstation on a config would share it and overwrite each other.
# Keep this in sync with the identical selection in 271_*.sh.
WS_ID="$(resolve_ws_id)" || log "WARN: metadata + cache both unavailable; using hostname=${WS_ID} (namespace may fork)"
PLUGIN_PREFIX="gs://${BUCKET}/claude-sync"

# Recency/age filter knobs (load-reduction plan #1). Namespaces accumulate
# one-per-workstation forever with no GC; the cross-namespace pull cost
# grew unbounded. We rank candidate namespaces by their manifest's
# last_push_at_epoch, then pull only those active within MAX_AGE_DAYS AND
# cap to the MAX_NAMESPACES most recent. Both are configurable via plugin
# config; defaults below match docs/adr/0004-load-reduction.md.
MAX_AGE_DAYS="${RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_AGE_DAYS:-30}"
MAX_NAMESPACES="${RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_NAMESPACES:-15}"
case "${MAX_AGE_DAYS}" in *[!0-9]*|"") MAX_AGE_DAYS=30 ;; esac
case "${MAX_NAMESPACES}" in *[!0-9]*|"") MAX_NAMESPACES=15 ;; esac

MODE="boot"; [ "${PERIODIC}" -eq 1 ] && MODE="periodic"
log "starting ${MODE} pull as ws-id=${WS_ID} from ${PLUGIN_PREFIX} (max_age_days=${MAX_AGE_DAYS} max_namespaces=${MAX_NAMESPACES})"

mkdir -p "${USER_HOME}/.claude"

# List namespaces. `gcloud storage ls` returns one
# `gs://bucket/claude-sync/workstations/<ws>/` per line; an
# empty/nonexistent prefix is not an error here.
mapfile -t NAMESPACES < <(gcloud storage ls "${PLUGIN_PREFIX}/workstations/" 2>>"${LOG_FILE}" | sort -u || true)

if [ "${#NAMESPACES[@]}" -eq 0 ]; then
    log "no namespaces under ${PLUGIN_PREFIX}/workstations/ — first boot"
    chown -R "${USER_NAME}:${USER_NAME}" "${USER_HOME}/.claude"
    exit 0
fi

# --- Recency/age-filtered + capped namespace selection (plan #1) -------------
# Without GC, every past `ra create` leaves an ~85 MB namespace behind, so
# an unfiltered pull rsyncs gigabytes across dozens of dead namespaces on
# every boot. Rank by each manifest's last_push_at_epoch and pull only the
# namespaces that are (a) newer than MAX_AGE_DAYS and (b) within the
# MAX_NAMESPACES most-recent. Our own WS_ID is always skipped (we push it,
# never pull it). A missing/invalid manifest is treated as epoch 0 — the
# oldest possible — so undated namespaces fall off first and never crowd
# out a dated, active one. Every exclusion is logged; nothing is silently
# truncated.
#
# now/cutoff in epoch seconds; cutoff=0 disables the age gate when
# MAX_AGE_DAYS=0 (cap-only mode).
now_epoch="$(date -u +%s)"
if [ "${MAX_AGE_DAYS}" -gt 0 ]; then
    cutoff_epoch=$(( now_epoch - MAX_AGE_DAYS * 86400 ))
else
    cutoff_epoch=0
fi

# Fetch all namespace manifests in ONE gcloud round-trip instead of one
# `gcloud storage cat` per namespace — ranking N namespaces otherwise pays
# N gcloud cold-starts (tens of seconds, and N chances to flake). Mirror
# them locally, best-effort: a namespace whose manifest is missing or
# unreadable falls through to epoch 0 below (treated as oldest), as before.
manifest_dir="$(mktemp -d)"
gcloud storage rsync --recursive \
    "${PLUGIN_PREFIX}/_manifest/" "${manifest_dir}/" 2>>"${LOG_FILE}" || true

# Build "<epoch>\t<namespace-uri>" rows for every candidate (excluding our
# own namespace), reading each manifest from the local mirror. Then sort
# newest-first and apply the TTL + cap.
ranked_rows=""
for ns in "${NAMESPACES[@]}"; do
    ns_id="${ns%/}"; ns_id="${ns_id##*/}"
    [ -z "${ns_id}" ] && continue
    if [ "${ns_id}" = "${WS_ID}" ]; then
        log "skip ${ns_id}: own namespace (push-only)"
        continue
    fi
    ts="$(jq -r '.last_push_at_epoch // 0' "${manifest_dir}/${ns_id}.json" 2>>"${LOG_FILE}" || echo 0)"
    ts="${ts:-0}"
    case "${ts}" in *[!0-9]*) ts=0 ;; esac
    ranked_rows+="${ts}	${ns}"$'\n'
done
rm -rf "${manifest_dir}"

# Sort by epoch descending (newest first). `sort -rn` keys on the leading
# numeric epoch field; the tab-separated URI rides along untouched.
mapfile -t ranked_sorted < <(printf '%s' "${ranked_rows}" | sort -rn -k1,1 | sed '/^$/d')

PULL_NS=()
declare -A NS_TS=()
SEEN_DIR="${CACHE_DIR}/seen"
mkdir -p "${SEEN_DIR}"
unchanged=0
kept=0
for row in "${ranked_sorted[@]}"; do
    ts="${row%%	*}"
    ns="${row#*	}"
    ns_id="${ns%/}"; ns_id="${ns_id##*/}"
    # Age gate (skipped when cutoff_epoch=0). ts=0 (missing/invalid
    # manifest) always fails a non-zero cutoff, so undated namespaces are
    # dropped first — exactly the intended "oldest" treatment.
    if [ "${cutoff_epoch}" -gt 0 ] && [ "${ts}" -lt "${cutoff_epoch}" ]; then
        if [ "${ts}" -eq 0 ]; then
            log "skip ${ns_id}: no/invalid manifest (treated as oldest, beyond ${MAX_AGE_DAYS}d TTL)"
        else
            log "skip ${ns_id}: last_push_at_epoch=${ts} older than ${MAX_AGE_DAYS}d TTL (cutoff=${cutoff_epoch})"
        fi
        continue
    fi
    # Recency cap.
    if [ "${kept}" -ge "${MAX_NAMESPACES}" ]; then
        log "skip ${ns_id}: beyond top-${MAX_NAMESPACES} recency cap (last_push_at_epoch=${ts})"
        continue
    fi
    kept=$((kept + 1))   # counts toward the cap even when skipped below
    NS_TS["${ns_id}"]="${ts}"
    # Periodic pulls skip a namespace whose manifest is exactly what we last
    # pulled: the manifest only advances after a successful push, so nothing
    # new can be in it. A running workstation advances its manifest every push
    # cycle and is always re-pulled; idle or stopped ones cost nothing. (The
    # boot pull never skips, and a missing manifest, ts=0, is never skipped.)
    if [ "${PERIODIC}" -eq 1 ] && [ "${ts}" -gt 0 ] \
        && [ "$(cat "${SEEN_DIR}/${ns_id}" 2>/dev/null || true)" = "${ts}" ]; then
        unchanged=$((unchanged + 1))
        continue
    fi
    PULL_NS+=("${ns}")
done

log "selected ${kept} of ${#NAMESPACES[@]} namespace(s) for pull (${unchanged} unchanged since the last pull)"
if [ "${kept}" -eq 0 ]; then
    log "no eligible namespaces after age/cap filter — nothing to pull"
    chown -R "${USER_NAME}:${USER_NAME}" "${USER_HOME}/.claude"
    exit 0
fi
if [ "${#PULL_NS[@]}" -eq 0 ]; then
    log "every selected namespace is unchanged since the last pull — nothing to do"
    exit 0
fi

# Stage everything so the existing live tree is unaffected until we
# cp-no-clobber / merge on top. The directory stage PERSISTS between pulls,
# so a periodic pull's `gcloud storage rsync` fetches only what changed
# instead of re-downloading every selected namespace each time. The
# single-file stage is rebuilt every run, so a namespace that has since fallen
# out of the age/cap selection can't keep feeding the history merge.
rm -rf "${STAGE}/history" "${STAGE}/claudejson"
mkdir -p "${STAGE}/history" "${STAGE}/claudejson"

for ns in "${PULL_NS[@]}"; do
    ns_id="${ns%/}"; ns_id="${ns_id##*/}"
    [ -z "${ns_id}" ] && continue
    log "staging namespace ${ns_id}"
    staged=1
    for sub in projects todos plans session-env; do
        mkdir -p "${STAGE}/${sub}"
        if ! gcloud storage rsync --recursive \
            "${ns}${sub}/" "${STAGE}/${sub}/" 2>>"${LOG_FILE}"; then
            # Only a failed projects/ sync counts against the namespace: the
            # others may legitimately not exist yet. A namespace that is not
            # recorded here is simply pulled again next time, never missed.
            [ "${sub}" = projects ] && staged=0
        fi
    done
    [ "${staged}" -eq 1 ] && printf '%s\n' "${NS_TS[${ns_id}]:-0}" > "${SEEN_DIR}/${ns_id}"
    # Single-file, merge-on-read members: stage one copy per namespace so
    # the union merge below can fold them all in.
    gcloud storage cp "${ns}history.jsonl" "${STAGE}/history/${ns_id}.jsonl" 2>>"${LOG_FILE}" || true
    if [ "${PERIODIC}" -eq 0 ]; then
        gcloud storage cp "${ns}claude.json" "${STAGE}/claudejson/${ns_id}.json" 2>>"${LOG_FILE}" || true
    fi
done

# Merge staged directory content into ~/.claude using `cp -an` (archive +
# no-clobber) so any newer local file wins. First-time fetches still
# populate empty subtrees.
for sub in projects todos plans session-env; do
    [ -d "${STAGE}/${sub}" ] || continue
    mkdir -p "${USER_HOME}/.claude/${sub}"
    cp -an "${STAGE}/${sub}/." "${USER_HOME}/.claude/${sub}/" 2>>"${LOG_FILE}" || true
done

# Union-merge the append-only / shared single files.
log "union-merging history.jsonl across $(ls -1 "${STAGE}/history" 2>/dev/null | wc -l) namespace(s) + local"
merge_jsonl_history "${USER_HOME}/.claude/history.jsonl" "${STAGE}"/history/*.jsonl

if [ "${PERIODIC}" -eq 0 ]; then
    log "merging .claude.json project history across $(ls -1 "${STAGE}/claudejson" 2>/dev/null | wc -l) namespace(s)"
    merge_claude_json "${USER_HOME}/.claude.json" "${STAGE}"/claudejson/*.json
fi

chown -R "${USER_NAME}:${USER_NAME}" "${USER_HOME}/.claude"
[ -f "${USER_HOME}/.claude.json" ] && chown "${USER_NAME}:${USER_NAME}" "${USER_HOME}/.claude.json"
[ -f "${WS_ID_CACHE}" ] && chown -R "${USER_NAME}:${USER_NAME}" "$(dirname "${WS_ID_CACHE}")" 2>/dev/null || true
log "done (${MODE})"
