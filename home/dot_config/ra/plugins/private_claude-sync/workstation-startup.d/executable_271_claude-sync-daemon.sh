#!/bin/bash
# claude-sync plugin — install + start the push loop.
#
# Materializes /usr/local/bin/ra-claude-sync-loop.sh and starts it.
# Tries the systemd path first (writes /etc/systemd/system/ra-claude-sync.service
# and enables/starts it). When systemd is not PID 1, falls back to a setsid-
# detached background process (mirroring the marimo plugin's daemon pattern).
#
# Why the fallback exists: Cloud Workstations' default base image runs
# `/google/scripts/entrypoint.sh` as PID 1 — NOT systemd — so `systemctl`
# calls succeed at the binary level but fail to register the unit with any
# init system. Before this fallback, the push loop was never starting in
# production: the unit file landed on disk and nothing ever exec'd it.
#
# Idempotent: re-running overwrites the loop script and (in fallback mode)
# kills the previous loop process before relaunching, so config changes
# (e.g. new interval_seconds) take effect on the next boot.
#
# Runs as root so it inherits the workstation service-account auth that
# gcloud picks up from the metadata server, and can read /home/user/.claude/
# regardless of file mode quirks. All loop chatter goes to
# /var/log/ra-claude-sync.log so operators can diagnose sync regressions
# without bouncing the workstation just to read the journal.
set -euo pipefail

[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_CLAUDE_SYNC_ENABLED:-false}" = "true" ] || exit 0

LOOP_PATH="/usr/local/bin/ra-claude-sync-loop.sh"
UNIT_PATH="/etc/systemd/system/ra-claude-sync.service"
LOG_FILE="/var/log/ra-claude-sync.log"
# Pidfile must agree with the path the loop script writes to. Lives in
# /run so it auto-clears on reboot — any leftover PID would refer to a
# vanished process anyway.
PID_FILE="/run/ra-claude-sync.pid"

cat > "${LOOP_PATH}" <<'LOOP_EOF'
#!/bin/bash
# ra-claude-sync push loop — installed by 271_claude-sync-daemon.sh.
#
# Waits for /run/ra/env (the core 200_*.sh boot script writes it on
# every boot), then pushes a filtered allowlist of /home/user/.claude/
# to gs://${BUCKET}/claude-sync/workstations/${WS_ID}/ every ${INTERVAL}
# seconds. All data lives under the claude-sync/ prefix so other plugins
# can share the same bucket without collisions.
#
# Push integrity (see SPEC.md "ADR 2026-06-07"):
#   * history.jsonl is sanitized (torn/invalid lines dropped) before
#     upload so a line caught mid-append never ships.
#   * .claude.json is JSON-validated before upload; a torn read is
#     skipped for the cycle.
#   * the manifest is advanced ONLY when every push in the cycle
#     succeeded, so a partial/failed push never advertises itself as
#     "freshest".
#
# All gcloud chatter is appended to /var/log/ra-claude-sync.log so a
# silent regression in steady state is auditable. Exits with status 1
# on any unrecoverable startup error (env never appears, plugin
# disabled at startup). Under systemd the Restart=always policy retries;
# under the setsid fallback the loop simply dies until the next
# workstation reboot re-runs 271_*.sh.
set -uo pipefail

USER_HOME="/home/user"
LOG_FILE="/var/log/ra-claude-sync.log"
PID_FILE="/run/ra-claude-sync.pid"
# Last-known-good workstation id, cached on the persistent home disk.
WS_ID_CACHE="${USER_HOME}/.ra-claude-sync/ws-id"

mkdir -p "$(dirname "${LOG_FILE}")" "$(dirname "${PID_FILE}")"
exec >> "${LOG_FILE}" 2>&1

# Record our PID so 271_*.sh can find and stop us idempotently on the
# next boot. Cleaned up on exit. The pidfile is informational under
# systemd (which tracks the process itself) but is the authoritative
# handle under the setsid fallback path.
echo $$ > "${PID_FILE}"
trap 'rm -f "${PID_FILE}"' EXIT

log() { echo "[claude-sync-loop] $(date -u +%H:%M:%S) $*"; }

# Resolve this workstation's stable id. Metadata is authoritative; we
# cache the last-known-good value and prefer it over the `hostname`
# fallback so a momentary metadata outage cannot fork our namespace.
# MUST stay in sync with the identical function in 270_claude-sync-pull.sh.
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

# Wait up to 5 minutes for /run/ra/env to be populated. On a fresh
# workstation boot, this systemd unit may start before the core
# workstation-startup.d/200_*.sh has run. After the timeout we exit 1
# so systemd's Restart=always kicks in with backoff instead of leaving
# the service quietly stalled.
ready=0
for _ in $(seq 1 60); do
    if [ -r /run/ra/env ]; then
        ready=1
        break
    fi
    sleep 5
done
if [ "${ready}" -ne 1 ]; then
    log "/run/ra/env not present after 5min; exiting 1 so systemd restarts us"
    exit 1
fi

set -a && . /run/ra/env && set +a

if [ "${RA_PLUGIN_CLAUDE_SYNC_ENABLED:-false}" != "true" ]; then
    log "plugin disabled at startup; exiting 1 so systemd restarts us if env later changes"
    exit 1
fi
if [ -z "${RA_PLUGIN_CLAUDE_SYNC_BUCKET:-}" ]; then
    log "RA_PLUGIN_CLAUDE_SYNC_BUCKET unset at startup; exiting 1"
    exit 1
fi

BUCKET="${RA_PLUGIN_CLAUDE_SYNC_BUCKET}"
INTERVAL="${RA_PLUGIN_CLAUDE_SYNC_INTERVAL_SECONDS:-60}"
case "${INTERVAL}" in *[!0-9]*|"") INTERVAL=60 ;; esac

# Namespace selection (load-reduction plan #3). Prefer the stable
# RA_PLUGIN_CLAUDE_SYNC_NAMESPACE injected by `ra create` so a rebuilt
# workstation pushes to ONE namespace instead of a fresh UUID namespace
# each create. Fall back to resolve_ws_id() (metadata / cached / hostname)
# when unset. Keep this block in sync with the identical selection in
# 270_*.sh. Metadata is up by now (we waited for /run/ra/env, well after
# boot), so the fallback resolves authoritatively and caches the value.
if [ -n "${RA_PLUGIN_CLAUDE_SYNC_NAMESPACE:-}" ]; then
    WS_ID="${RA_PLUGIN_CLAUDE_SYNC_NAMESPACE}"
    log "using stable namespace from RA_PLUGIN_CLAUDE_SYNC_NAMESPACE=${WS_ID}"
else
    WS_ID="$(resolve_ws_id)" || log "WARN: metadata + cache both unavailable; using hostname=${WS_ID} (namespace may fork)"
fi

PREFIX="gs://${BUCKET}/claude-sync/workstations/${WS_ID}"
MANIFEST_PATH="gs://${BUCKET}/claude-sync/_manifest/${WS_ID}.json"

log "starting push loop ws-id=${WS_ID} interval=${INTERVAL}s prefix=${PREFIX}"

# Upload history.jsonl with torn/invalid lines stripped. Returns non-zero
# only if the upload itself failed (so the caller can withhold the
# manifest); a file that is absent or has no valid lines is a no-op success.
push_history() {
    local src="${USER_HOME}/.claude/history.jsonl" tmp
    [ -f "${src}" ] || return 0
    tmp="$(mktemp)"
    jq -R -c 'fromjson? // empty' "${src}" > "${tmp}" 2>>"${LOG_FILE}" || true
    if [ -s "${tmp}" ]; then
        if ! gcloud storage cp "${tmp}" "${PREFIX}/history.jsonl"; then
            rm -f "${tmp}"; return 1
        fi
    else
        log "history.jsonl had no valid lines this cycle; skipping its push"
    fi
    rm -f "${tmp}"
    return 0
}

# Upload .claude.json only if it is currently valid JSON (a mid-write read
# is skipped). A skipped invalid read is not a push failure.
push_claude_json() {
    local src="${USER_HOME}/.claude.json"
    [ -f "${src}" ] || return 0
    if jq -e . "${src}" >/dev/null 2>&1; then
        gcloud storage cp "${src}" "${PREFIX}/claude.json" || return 1
    else
        log ".claude.json not valid JSON this cycle; skipping its push"
    fi
    return 0
}

push_once() {
    local ok=1 sub
    for sub in projects todos plans session-env; do
        if [ -d "${USER_HOME}/.claude/${sub}" ]; then
            gcloud storage rsync --recursive \
                "${USER_HOME}/.claude/${sub}/" "${PREFIX}/${sub}/" || ok=0
        fi
    done
    push_history    || ok=0
    push_claude_json || ok=0

    # Manifest carries last_push_at so operators (and a future v2
    # incremental pull) can see when each namespace last fully synced.
    # Advance it ONLY on a fully-successful cycle so it never points at a
    # namespace whose data upload failed.
    if [ "${ok}" -eq 1 ]; then
        local ts iso tmp
        ts=$(date -u +%s)
        iso=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
        tmp=$(mktemp)
        printf '{"last_push_at": "%s", "last_push_at_epoch": %s}\n' "${iso}" "${ts}" > "${tmp}"
        gcloud storage cp "${tmp}" "${MANIFEST_PATH}" || log "manifest upload failed"
        rm -f "${tmp}"
    else
        log "push cycle incomplete; not advancing manifest"
    fi
}

while true; do
    push_once
    sleep "${INTERVAL}"
done
LOOP_EOF
chmod 0755 "${LOOP_PATH}"

# PID 1 detection. Cloud Workstations' default image runs
# /google/scripts/entrypoint.sh as PID 1, so /proc/1/comm reads as
# "entrypoint.sh", not "systemd". Note: `systemctl` may still be on
# PATH (Dockerfile.d's soft-assert doesn't catch this) but without
# systemd-as-PID-1 the daemon-reload / enable calls are no-ops.
have_systemd_pid1() {
    [ "$(cat /proc/1/comm 2>/dev/null)" = "systemd" ]
}

start_via_systemd() {
    cat > "${UNIT_PATH}" <<'UNIT_EOF'
[Unit]
Description=ra claude-sync push loop (sync ~/.claude to GCS)
After=network-online.target
Wants=network-online.target
# Restart=always combined with a generous burst window: if startup
# fails (env not ready, plugin disabled), we want systemd to keep
# retrying with backoff rather than parking the unit silently.
StartLimitIntervalSec=600
StartLimitBurst=20

[Service]
Type=simple
ExecStart=/usr/local/bin/ra-claude-sync-loop.sh
Restart=always
RestartSec=30

[Install]
WantedBy=multi-user.target
UNIT_EOF

    systemctl daemon-reload
    # --no-block so we don't wait on the unit's initial cycle.
    systemctl enable --now --no-block ra-claude-sync.service || true
    # `restart` so subsequent `ra create` runs that mutate env take effect.
    systemctl restart ra-claude-sync.service || true
    echo "[claude-sync-daemon] ra-claude-sync.service installed and started (logs: ${LOG_FILE})"
}

start_via_setsid() {
    mkdir -p "$(dirname "${LOG_FILE}")"

    # Idempotency: a re-run of 271_*.sh (or a manual invocation) should
    # not produce two parallel loops. The loop script writes its own PID
    # to PID_FILE on start (cleared on exit). Use that PID directly rather
    # than pgrep-by-pattern, which would also match the calling shell's
    # command line whenever the loop path happens to appear in it (e.g. a
    # shell session that just `cat`ed the script).
    if [ -f "${PID_FILE}" ]; then
        old_pid="$(cat "${PID_FILE}" 2>/dev/null || true)"
        # Validate before kill: empty, non-numeric, or dead-PID values
        # all fall through to the harmless `rm -f` below. Matches the
        # case-pattern style used elsewhere in this file (see INTERVAL
        # validation in the loop body).
        case "${old_pid}" in
            ''|*[!0-9]*)
                ;;
            *)
                if kill -0 "${old_pid}" 2>/dev/null; then
                    echo "[claude-sync-daemon] stopping existing loop pid=${old_pid}"
                    kill "${old_pid}" 2>/dev/null || true
                    # Give the previous loop up to 5s to exit cleanly before SIGKILL.
                    for _ in 1 2 3 4 5; do
                        kill -0 "${old_pid}" 2>/dev/null || break
                        sleep 1
                    done
                    kill -9 "${old_pid}" 2>/dev/null || true
                fi
                ;;
        esac
        rm -f "${PID_FILE}"
    fi

    # setsid --fork detaches the loop into its own session/process-group
    # so it survives this startup script's exit (matches the marimo plugin
    # pattern). The loop script does its own log redirection internally,
    # writes its PID to PID_FILE, and clears it on exit, so we don't need
    # to capture the PID here.
    setsid --fork "${LOOP_PATH}" </dev/null
    echo "[claude-sync-daemon] loop started via setsid (PID 1 is $(cat /proc/1/comm 2>/dev/null || echo unknown); logs: ${LOG_FILE})"
}

if have_systemd_pid1; then
    start_via_systemd
else
    start_via_setsid
fi
