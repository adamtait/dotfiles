# 2. Merge-based sync for append-only / shared files

- Status: accepted (2026-06-07)
- Supersedes: the last-writer-wins whole-file handling of `history.jsonl` and `.claude.json` in the
  original v1 data flow (ADR 0001)
- Applies to: `claude-sync` plugin

## Context

An incident review found session entries that existed in the GCS bucket but were missing from a
workstation's local `history.jsonl` (measured: ~21 entries missing locally; across 25 namespaces the
per-file line counts ranged 46–127, union = 148 unique). Root cause was a design flaw, not a typo:
`history.jsonl` is an **append-only union log** — one line per session prompt, keyed by `sessionId` —
but each workstation only ever appends the sessions *it* ran, so every namespace holds a **divergent
partial copy**. The v1 boot pull picked the single namespace with the newest manifest timestamp and
**whole-file-overwrote** local `history.jsonl` with that partial copy, destroying every entry the
"winner" lacked. `.claude.json` was overwritten the same way.

Three contributing bugs amplified it:

1. **Manifest advanced on failure.** Every push op had `|| true` and then the manifest was written
   unconditionally, so a namespace whose data upload failed still advertised itself as "freshest," and
   the pull would trust its stale/missing data.
2. **Unstable `WS_ID`.** The id fell back from GCE metadata to `hostname` on any metadata blip; the two
   differ, so a single flap forked one workstation's history into a second namespace, which
   last-writer-wins then dropped.
3. **Torn reads.** A Claude append concurrent with the streaming rsync read could ship a torn last
   line, which then propagated.

## Decision

- **`history.jsonl` → union-merge on pull, sanitize on push.** The pull stages *every* namespace's
  copy and merges local + all: drop torn lines (`jq -R 'fromjson? // empty'`), dedup by full object,
  sort by timestamp. The push filters through `fromjson? // empty` before upload so a torn line is
  never published. The merge is namespace-order-independent and idempotent, and recovers previously
  lost entries on the next boot. Write-back is atomic (temp file in the destination's own directory +
  rename) and skipped if the merge yields empty, so a `jq` failure never wipes a good file.
- **`.claude.json` → local base + per-project history union.** The local file stays authoritative for
  every non-history field (machine-local `oauthAccount`, onboarding flags, `numStartups`,
  `tipsHistory`); only per-project `history` arrays are folded in from other namespaces,
  order-preserving dedup. Foreign config is never imported — the old whole-file overwrite could clobber
  this machine's auth/onboarding, which this fixes. The merge only touches projects where the remote
  has a non-empty `history` array, so it never creates empty `{"history":[]}` project stubs. (Chosen
  over "stop syncing it" and "leave last-writer-wins".)
- **Manifest honesty (bug 1).** The push tracks the exit status of every gcloud op and advances the
  manifest only on a fully-successful cycle. A *skipped* push (e.g. `.claude.json` invalid this cycle)
  is deliberately not counted as a failure, so a transient torn read never freezes the manifest. Since
  the union merge removed the pull's dependence on the manifest, this is now defense-in-depth +
  observability rather than load-bearing.
- **Stable `WS_ID` (bug 2).** A single `resolve_ws_id`: metadata (with brief retry, authoritative) →
  last-known-good cache at `/home/user/.ra-claude-sync/ws-id` → `hostname` only if both fail. Hostname
  results are **never cached** (only authoritative metadata is), so once metadata recovers the real id
  is restored. The same logic is used in both `270_*.sh` and the push loop so they cannot disagree.
- **Torn-read tolerance (bug 3).** Push sanitizes `history.jsonl` and validates `.claude.json` before
  upload; pull drops unparseable lines and skips an invalid `.claude.json` source during the merge.

## Consequences

- The boot pull now downloads every namespace's `history.jsonl` and `.claude.json` rather than one — at
  ~25 namespaces, ~1 MB of extra download, negligible but linear in namespace count. A future version
  can use the (now-honest) manifest to skip unchanged namespaces. (Namespace count is separately bounded
  by ADR 0004.)
- `jq` is load-bearing on the pull path (installed by the plugin's `Dockerfile.d/` tools step).
- The merge is union-only — no deletions ever propagate — consistent with the additive sync model.
- The `WS_ID` cache lives on the persistent home disk (Cloud Workstations only persist `/home`), outside
  `~/.claude` and off the push allowlist, so it is never synced. A first boot with metadata down (no
  cache yet) can still fork once to a `hostname` namespace; it self-heals when metadata returns. A
  fully fail-closed "refuse to sync" alternative was rejected because it would also break local/dev
  testing where metadata never exists.
- The 21 already-lost entries needed no one-shot backfill script: the next boot pull folds the bucket
  copies back in, so the next boot *is* the backfill.
- **Review-driven refinements applied:** the merge temp files were originally created with bare
  `mktemp` (→ `/tmp`), but on Cloud Workstations `/tmp` and `/home` are different mounts, so the
  write-back was a non-atomic copy a crash could truncate mid-write — fixed by creating the temp file in
  the destination directory so the write-back is a true rename. History dedup was also switched from a
  4-tuple key to full-object `unique`, so a genuinely distinct entry sharing that tuple is never
  dropped (cross-namespace duplicates are byte-identical, so exact-object dedup is sufficient and
  strictly safer).
