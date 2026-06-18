# SPEC: Sync `~/.claude` state across cloud workstations

## Context

The `ra` CLI provisions Google Cloud Workstations whose claude plugin installs Claude Code at `/home/user/.npm-global/bin/claude`. When claude runs, it writes local state to `/home/user/.claude/` and `/home/user/.claude.json` — session transcripts under `projects/<cwd>/<sessionId>.jsonl`, the global config in `.claude.json`, a global command log `history.jsonl`, CLAUDE.md memory files inside `projects/<cwd>/memory/`, todos, plans, etc. That state currently lives only on the workstation's boot disk: it survives stop/start but is **destroyed when the workstation is deleted**, and **never crosses between workstations**. Users who create a new workstation lose all prior session context.

We want to share session history, config, and memory across workstations belonging to the same user/project, with these constraints (chosen up-front):

- Often-parallel workstations writing concurrently → need per-workstation namespacing to avoid corruption.
- Sync scope: history + config + memory (`projects/`, `history.jsonl`, `.claude.json`, CLAUDE.md memory, todos, plans).
- Backend: GCS — fits the existing "shell out to gcloud" pattern, no new VPC/NFS infra.

The intended outcome: when a user creates a new workstation, prior session transcripts and CLAUDE.md memory are present; when they work concurrently from two workstations, neither corrupts the other; when they delete a workstation, its history survives in GCS.

---

## Simplest possible version (v1)

A new `claude-sync` plugin (separate from `claude`, but depending on it) that:

1. **On every boot**, after `260_claude-setup.sh`, pulls every workstation's namespace from a per-user GCS bucket and merges them into the local `~/.claude/`.
2. **Continuously**, runs a 60-second background loop (`systemd` unit) that `gcloud storage rsync`'s a filtered view of `~/.claude/` → `gs://<bucket>/workstations/<this-workstation-id>/`.
3. **No shutdown hook** in v1 — the worst-case data loss is ≤60s of state, which is acceptable given session JSONL files are append-only and the next pull picks them up if the workstation restarts.

Per-workstation namespacing in GCS means concurrent writes from different workstations never touch the same object. Merging happens on read.

GCS bucket is provisioned by `ra create` (idempotent, like the Artifact Registry repo) when `claude-sync` is installed, and the workstation service account gets `roles/storage.objectAdmin` scoped to that bucket.

---

## Architecture diagram

```
 ┌──────────────────────────── Workstation A ────────────────────────────┐
 │                                                                       │
 │   /home/user/.claude/   (boot disk, ephemeral on workstation delete)  │
 │   ├── projects/<cwd>/<sessionId>.jsonl    ← claude writes session log │
 │   ├── projects/<cwd>/memory/*.md          ← CLAUDE.md memory          │
 │   ├── history.jsonl                       ← global command log        │
 │   └── .claude.json                        ← config + onboarding state │
 │             ▲                  │                                      │
 │   pull  (boot, once)      push (every 60s)                            │
 │             │                  ▼                                      │
 │   ┌───────────────────┐  ┌────────────────────────┐                   │
 │   │ 270_claude-sync-  │  │ ra-claude-sync.service │                   │
 │   │ pull.sh           │  │ (systemd → gcloud      │                   │
 │   │ (rsync merge in)  │  │  storage rsync out)    │                   │
 │   └───────────────────┘  └────────────────────────┘                   │
 │             │                  │                                      │
 └─────────────┼──────────────────┼──────────────────────────────────────┘
               │ workload-identity SA (roles/storage.objectAdmin on bucket)
               ▼                  ▼
 ┌─────────────────────────────────────────────────────────────────────┐
 │  GCS bucket: ra-<project-id>-<user-hash>  (shared with other plugins)│
 │                                                                     │
 │  claude-sync/workstations/<ws-id-A>/                                │
 │  ├── projects/<cwd>/<sessionId-A>.jsonl                             │
 │  ├── projects/<cwd>/memory/*.md                                     │
 │  ├── history.jsonl                                                  │
 │  └── claude.json                                                    │
 │  claude-sync/workstations/<ws-id-B>/...                             │
 │  claude-sync/workstations/<ws-id-C>/...                             │
 │  claude-sync/_manifest/<ws-id>.json   (last-push timestamp)         │
 └─────────────────────────────────────────────────────────────────────┘
               ▲                  ▲
               │ same pattern     │
 ┌─────────────┼──────────────────┼──────────────────────────────────────┐
 │             ▼                  ▼                                      │
 │   Workstation B (same components, different namespace under ws-id-B)  │
 └───────────────────────────────────────────────────────────────────────┘
```

---

## Components

### New plugin: `plugins/claude-sync/`

Mirrors the layout of `plugins/github/`. Files:

- **`plugin.yaml`** — config schema:
  - `enabled` (bool, default false)
  - `interval_seconds` (int, default 60)
  - `bucket_name_override` (string, optional — if unset, derived as `ra-<project-id>-<user-hash>`)
  - `sync_all_projects` (bool, default false — at user-level install, auto-sets `bucket_name_override` to the user project's bucket so all GCP projects share one bucket)
  - No secrets — auth is via the existing workstation service account.
- **`Dockerfile.d/110_claude-sync-tools.sh`** — ensures `jq` and `curl` are present (already in base image — script is a `command -v` guard + no-op).
- **`workstation-startup.d/270_claude-sync-pull.sh`** — runs once per boot after `260_claude-setup.sh`. Pulls every namespace under `workstations/` and merges into `/home/user/.claude/`. Idempotent.
- **`workstation-startup.d/271_claude-sync-daemon.sh`** — heredocs the systemd unit + push loop into `/etc/systemd/system/` + `/usr/local/bin/`, then enables/starts the unit. Idempotent.
- **systemd unit** `/etc/systemd/system/ra-claude-sync.service` — runs `/usr/local/bin/ra-claude-sync-loop.sh` as `user`.
- **push loop** `/usr/local/bin/ra-claude-sync-loop.sh`:
  ```bash
  while true; do
    gcloud storage rsync \
      --recursive --delete-unmatched-destination-objects=false \
      --exclude='shell-snapshots/**|sessions/**|plugins/**|cache/**|backups/**|statsig/**' \
      /home/user/.claude/ \
      "gs://${BUCKET}/workstations/${WS_ID}/" || true
    sleep "${INTERVAL}"
  done
  ```

### `ra` CLI changes — **mechanism only, no claude-sync-specific code**

Separation of concerns: the **policy** ("claude-sync needs a bucket called X with role Y bound to env var Z") lives in `plugins/claude-sync/plugin.yaml`. The **mechanism** (the `gcloud storage` shell-outs and the create-time pipeline step) lives in core. Same pattern the codebase already uses for `auth_providers[].iam_roles:` (declared in plugin.yaml, walked generically in `pluginAuthIAMRoles`) and `port-for-tunnel:` (declared in plugin.yaml, served by `Registry.PortByName`). Core has **zero** plugin-specific name hard-codes.

- **`internal/plugins/schema.go`** — new `BucketSpec` field on `PluginSchema`:
  ```yaml
  buckets:
    - name: <logical-handle>
      name_template: "..."        # supports {project_id}, {user_email_hash}
      override_field: <config-field-name>  # optional; if set in plugins.<name>.*, overrides the template
      role: roles/storage.objectAdmin       # IAM role granted on the bucket to the workstation SA
      env_var: RA_PLUGIN_<NAME>_BUCKET      # env var the bucket name is exposed under
  ```
- **`internal/gcs/bucket.go`** — `EnsureBucket`, `ValidateBucketName`, `ResolveBucketName(template, project, userEmail)`, and a generic `GrantRole(r, attempts, delay, notice, bucket, sa, role)`. Generic GCP plumbing, parallel to `internal/secrets/`.
- **`cmd/create.go`** — a single `pluginBuckets(reg.All(), cfg)` helper enumerates every enabled plugin's `BucketSpec` entries (resolved to concrete names). The provisioning step, the IAM bind, and the env injection in `buildContainerEnv` all iterate this list. No string literal "claude-sync" appears anywhere in core.

### `plugins/claude-sync/plugin.yaml` carries the policy

```yaml
name: claude-sync
buckets:
  - name: state
    name_template: "ra-{project_id}-{user_email_hash}"
    override_field: bucket_name_override
    role: roles/storage.objectAdmin
    env_var: RA_PLUGIN_CLAUDE_SYNC_BUCKET
config:
  - { name: enabled, type: bool, default: false }
  - { name: interval_seconds, type: int, default: 60 }
  - { name: bucket_name_override, type: string }
  - { name: sync_all_projects, type: bool, default: false }
```

The runtime contract on the workstation is unchanged: `270_*.sh` and `271_*.sh` read `RA_PLUGIN_CLAUDE_SYNC_BUCKET` exactly as before. Only the *source of truth* for what env var to inject moved from Go into YAML.

### Reused existing utilities

- `internal/gcloud/runner.go` — shell pattern for gcloud calls.
- `internal/gcloud/retry.go` — `RetryOnSANotFound` for IAM bindings against freshly-created SAs.
- `internal/configwrite/append.go` — `MergePluginConfig` already handles registering the plugin's config under `plugins.claude-sync.*`.

---

## Data flow

### Boot sequence (numbers are `workstation-startup.d/` prefixes)

1. `200_remote-agent-setup.sh` (core) — fetches secrets, writes `/run/ra/env`, propagates env to `/etc/environment`.
2. `250_claude-install.sh` (claude plugin) — installs claude on first boot.
3. `260_claude-setup.sh` (claude plugin) — auth setup, seeds onboarding.
4. **`270_claude-sync-pull.sh` (new)** — for each `gs://<bucket>/claude-sync/workstations/<ws-id>/` namespace:
   - rsync **projects/** down → local `~/.claude/projects/`. Session JSONL filenames are UUIDs and don't collide across workstations. CLAUDE.md memory files inside `projects/<cwd>/memory/` are last-writer-wins by mtime (gcloud rsync respects mtime).
   - rsync **todos/**, **plans/**, **session-env/** down — also UUID-keyed, no collisions.
   - For **history.jsonl**, **union-merge**: fold local + **every** namespace's copy together, drop torn/invalid lines (`jq -R 'fromjson? // empty'`), dedup by `(timestamp, sessionId, display, project)`, sort by `timestamp`. `history.jsonl` is an append-only union log (one entry per session prompt); each workstation only ever sees its own sessions, so every namespace holds a *divergent partial copy*. Whole-file overwrite — the original v1 behavior — silently drops every entry not present in the "winning" copy. It must be merged, never replaced. **This is the fix for the session-loss bug** (see ADR 2026-06-07 below).
   - For **.claude.json**, keep the **local** file as the authoritative base (it carries machine-local state: `oauthAccount`, onboarding flags, `numStartups`, `tipsHistory`) and union in only the per-project `history` arrays from every namespace. Foreign config is never imported — importing another workstation's `.claude.json` wholesale could clobber this machine's auth/onboarding.
   - The boot pull **no longer reads `_manifest/<ws-id>.json` to pick a "winning" namespace** — the union merge is namespace-order-independent, so there is nothing to rank. The manifest is retained only for observability and future incremental-pull (v2).
5. **`271_claude-sync-daemon.sh` (new)** — installs and starts the systemd unit.

### Steady state

- Every 60s, the daemon `gcloud storage rsync`'s the filtered view of `~/.claude/` → `gs://<bucket>/claude-sync/workstations/<this-ws-id>/`.
  - **`history.jsonl` is sanitized before upload**: it is filtered through `jq -R 'fromjson? // empty'` so a line torn by a concurrent claude append is never published; the next cycle re-ships the complete line.
  - **`.claude.json` is validated** (`jq -e .`) before upload; an invalid (mid-write) read is skipped that cycle.
- The manifest `_manifest/<this-ws-id>.json` is **only advanced when every push in the cycle succeeded**. A partial/failed push no longer advertises itself as "freshest" (see ADR 2026-06-07).
- Rsync is incremental: it only re-uploads files whose contents (md5) changed. Steady-state bandwidth is small.

### Shutdown

- No explicit hook. ≤60s of state may be lost if the workstation is killed between cycles. Acceptable since JSONLs are append-only and a new workstation re-syncs to the last persisted offset.

---

## ADR 2026-06-07 — Merge-based sync for append-only/shared files

**Status:** accepted, supersedes the last-writer-wins-overwrite handling of `history.jsonl` and `.claude.json` described in the original v1 data flow.

### Context

An incident review found ≥21 session entries that existed in the GCS bucket but were missing from a workstation's local `history.jsonl`. Root cause: `history.jsonl` is an append-only union log (one line per session prompt, keyed by `sessionId`), but every workstation only ever appends the sessions *it* ran — so each namespace holds a **divergent partial copy** (measured: 46–127 lines across 25 namespaces; union = 148 unique). The v1 boot pull picked the single namespace with the newest `_manifest` timestamp and **whole-file-overwrote** local `history.jsonl` with that partial copy, destroying every entry the "winner" lacked. `.claude.json` was overwritten the same way. Three contributing bugs amplified this:

1. The manifest timestamp was advanced even when the data upload failed (`|| true` on every `gcloud cp`), so the pull could elect a namespace whose data was stale/missing.
2. `WS_ID` fell back from GCE metadata to `hostname` on any metadata blip; the two differ (`workstations-…` vs `personal`), so a single flap forked one workstation's history into two namespaces, which last-writer-wins then dropped.
3. A claude append concurrent with the streaming rsync read could ship a torn last line, which then propagated.

### Decision

- **`history.jsonl` → union-merge on pull.** Fold local + every namespace's copy, drop torn lines (`jq -R 'fromjson? // empty'`), dedup by `(timestamp, sessionId, display, project)`, sort by `timestamp`. Namespace-order-independent and idempotent; recovers all previously-lost entries on the next boot. Write-back is atomic (`mktemp` + `mv`) and skipped if the merge yields empty (never wipes a good file on a jq failure).
- **`.claude.json` → local base + history union.** The local file stays authoritative for all non-history fields (machine-local auth/onboarding/counters); only per-project `history` arrays are unioned in from other namespaces, order-preserving dedup. Foreign config is never imported. (Chosen over "stop syncing it" and "leave LWW" — see implementation-notes.)
- **Manifest honesty (bug 1).** `push_once` tracks the exit status of every `gcloud` op and advances `_manifest/<ws>.json` only on a fully-successful cycle. The pull no longer *depends* on the manifest for correctness (the union merge removed that dependency), so this is now defense-in-depth + observability rather than load-bearing.
- **Stable `WS_ID` (bug 2).** A single resolver — metadata (with brief retry, authoritative) → last-known-good cache at `/home/user/.ra-claude-sync/ws-id` (persistent home disk) → `hostname` only if both fail. Identical logic in `270` and the push loop so they cannot disagree.
- **Torn-read tolerance (bug 3).** Push sanitizes `history.jsonl` and validates `.claude.json` before upload; pull drops unparseable lines / skips invalid `.claude.json` sources during the merge.

### Consequences

- Boot pull now downloads every namespace's `history.jsonl` and `.claude.json` (small + medium files) rather than one namespace's. At ~25 namespaces this is tens of MB of `.claude.json`; acceptable for boot, and a candidate for manifest-driven skip-unchanged in v2.
- `jq` is now load-bearing on the pull path (already installed by `Dockerfile.d/110`).
- The merge is union-only (no deletions ever propagate), consistent with the rest of the additive sync model.

---

## Tech stack

- **Go** — for the small `cmd/create.go` / `internal/gcs/` additions.
- **Bash** — workstation-side pull and push scripts (consistent with existing plugin scripts).
- **systemd** — provides the daemon lifecycle. Cloud Workstations' base image runs systemd inside the container.
- **`gcloud storage rsync`** — the actual sync engine. Already available in the base image (the workstation service account has cloud-platform scope).
- **GCS** — storage backend. One bucket per (project, user). Uniform bucket-level access. Default standard storage class. Versioning off for v1; enable in v2.
- **IAM** — `roles/storage.objectAdmin` granted on the bucket to the workstation SA.

---

## Edge cases

- **Session JSONL collisions**: impossible — claude only writes to its own active session's JSONL (UUID-named).
- **`history.jsonl` entry loss** (the original v1 bug): each workstation's `history.jsonl` is a partial view, and v1 overwrote local with one namespace's copy on boot → any session not in that copy vanished. **Fixed**: union-merge on pull (and per-cycle sanitize on push). See ADR 2026-06-07.
- **CLAUDE.md memory file collisions** (same `projects/<cwd>/memory/notes.md` edited on two workstations simultaneously): last-writer-wins by mtime. v1 documents this; v2 can surface conflicts via `ra claude sync status`.
- **`.claude.json` divergence** (numStartups, tipsHistory counters drift per workstation): non-history fields are kept **local** (never imported from other namespaces); only per-project `history` arrays are unioned in. No machine's auth/onboarding can be clobbered by another's.
- **Workstation killed mid-rsync**: `gcloud storage rsync` uploads whole files (not atomic across the set), so the destination namespace may be partially updated. Acceptable: next cycle reconciles, and the manifest only advances on a fully-successful cycle. Pull-side merge tolerates partial namespaces.
- **Secrets leaking into transcripts**: session JSONLs may contain shell output, tool outputs, and pasted content. The bucket inherits project-level encryption and is IAM-locked to the workstation SA + project owners. Document this risk in the plugin README; recommend CMEK in v2.
- **Bucket name globally unique**: derive as `ra-<project-id>-<sha1(user-email)[:8]>` to avoid collisions and not leak identifying info. Allow override via `bucket_name_override`. The name intentionally omits "claude" so other plugins can share the same bucket under their own prefixes.
- **Stale namespaces from deleted workstations**: previously lingered in GCS forever and were merged on every boot. **Mitigated** by the load-reduction work (see "Load reduction" below): the boot pull now age/cap-filters namespaces and a bucket lifecycle rule cheapens/deletes old data. Manual cleanup is still `gcloud storage rm -r gs://.../workstations/<old-ws-id>/` plus the matching `_manifest/<old-ws-id>.json`.
- **Namespace sprawl from per-create UUIDs**: `WS_ID` defaulted to the GCE instance name, a fresh UUID every `ra create`, so a workstation rebuilt N times left N namespaces (~85 MB each) that nothing pruned. **Mitigated** by the stable namespace (load-reduction #3): `ra create` injects `RA_PLUGIN_CLAUDE_SYNC_NAMESPACE`, derived from (project, config_name, user_email), and both scripts prefer it — a rebuild reuses ONE namespace.
- **`claude-sync` enabled but `claude` plugin missing**: documented in the plugin README. v1 noops if `/home/user/.claude/` doesn't exist.
- **Concurrent claude write + rsync read**: `gcloud storage rsync` does a streaming read of each file; a partial write could ship a torn JSONL/JSON. **Mitigated**: `history.jsonl` is sanitized before push and torn lines are dropped on merge; `.claude.json` is JSON-validated before push and skipped on merge if invalid.
- **First-ever boot, empty bucket**: pull is a no-op. Push initializes the namespace.
- **Bucket creation race when two workstations boot simultaneously**: `gcloud storage buckets create` is idempotent against describe pre-check.
- **Workstation identity (`WS_ID`) instability**: metadata is authoritative and the last-known-good value is cached on the persistent home disk (`/home/user/.ra-claude-sync/ws-id`); a transient metadata blip reuses the cache instead of falling through to `hostname` and forking the namespace. `hostname` is a true last resort only when metadata is down *and* no cache exists (first boot). See ADR 2026-06-07.

---

## Scaling strategy

- **Storage**: ~10–50 MB per workstation; 100 MB-ish total per user. Negligible at GCS pricing (~$0.02/GB/mo).
- **Sync frequency**: 60s default; configurable per plugin install. Users running interactive long sessions can lower to 10s.
- **Bandwidth**: rsync only reships changed files. Once warm, an active 60-min claude session pushes maybe 1–5 MB total.
- **Workstation count**: pull-merge cost is O(N × files). At 10 workstations × 1000 files, pull takes <30s on cold boot. Beyond ~50 workstations, switch to manifest-driven incremental pull (v2).
- **Per-user, per-project isolation**: one bucket per project per user keeps blast radius small.

---

## Possible bottlenecks

- **`gcloud storage rsync` cold-start latency** (~1–2s per invocation): noticeable if interval shrinks below 10s. Mitigation in v2: long-lived Go daemon using the GCS client library + inotify.
- **JSONL growth on long-running sessions**: a 50 MB session file gets re-uploaded whole on every change because rsync compares md5. Mitigation in v2: chunked append-only object writes.
- **Pull-merge cold boot time** at 10+ workstations: linear scan. Mitigation in v2: read `_manifest/*.json` first, skip unchanged namespaces.
- **systemd inside the Cloud Workstations container**: if not available, fall back to a `nohup` background loop launched from `271_*.sh`.
- **gcloud auth at boot**: workstation SA is already active before `271_*.sh` runs (existing `200_remote-agent-setup.sh` uses gcloud successfully).

---

## Load reduction (2026-06)

By mid-2026 the sync bucket held ~36 namespaces / ~2.2 GB (~85 MB each)
and was growing without bound: `WS_ID` defaulted to the ephemeral GCE
instance name (new every `ra create`) and nothing ever pruned. The
detached boot pull (PR #5) stopped the pull from breaking boot, but the
background pull cost still scaled with the dead-namespace count. Three
changes bound the cost.

### #1 — Recency/age-filtered + capped boot pull (`270_*.sh`)

Before staging, the pull ranks every candidate namespace by its
`_manifest/<ns>.json` `last_push_at_epoch` and pulls only those that are
**both** newer than `pull_max_age_days` **and** within the
`pull_max_namespaces` most-recent. Our own `WS_ID` is always skipped
(push-only). A missing/invalid manifest is treated as epoch 0 — the
oldest possible — so undated namespaces fall off first and never crowd
out a dated, active one. Every exclusion is logged (own-namespace,
beyond-TTL, no-manifest, beyond-cap); nothing is silently truncated.

Config keys (surfaced to the script as
`RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_AGE_DAYS` /
`RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_NAMESPACES`):

| Key                   | Default | Meaning                                                        |
|-----------------------|---------|----------------------------------------------------------------|
| `pull_max_age_days`   | `30`    | Only merge namespaces pushed within this many days. `0` = no age limit (cap-only). |
| `pull_max_namespaces` | `15`    | Cap the merge to the N most-recently-pushed namespaces.        |

The last-writer-wins / union-merge model is unchanged — it just operates
over the filtered set. Historical UUID namespaces are still merged from
until they age out of the window, then they stop being read at all.

### #3 — Stable per-user namespace (`ra` core + `270_*.sh` + `271_*.sh`)

`ra create`'s `buildContainerEnv` injects
`RA_PLUGIN_CLAUDE_SYNC_NAMESPACE`, a deterministic id derived from
`(gcloud project, workstation config_name, git user_email)` — lowercased,
restricted to `[a-z0-9_-]`, slash-free, and suffixed with an 8-hex digest
of the raw triple so distinct triples never collide. Injection is gated on
the plugin being enabled and added to `RA_PROPAGATE_KEYS`. Both scripts
prefer this env var over `resolve_ws_id()` and fall back to the metadata/
cache/hostname chain only when it is unset.

**Migration**: existing UUID namespaces remain in the bucket as read-only
history. The filtered pull (#1) keeps merging from them until they age out
of the `pull_max_age_days` / `pull_max_namespaces` window; meanwhile the
workstation pushes only to the new stable namespace. No data is moved or
deleted as part of enabling #3 — the old namespaces simply stop receiving
writes and eventually stop being read, then the lifecycle rule (#4)
reclaims them.

### #4 — Bucket Object Lifecycle Management (`plugin.yaml` + `ra` core)

The `state` bucket's `buckets[].lifecycle:` declaration carries two rules,
both scoped to the `claude-sync/` prefix via `matchesPrefix` so they never
touch objects other plugins write into the shared bucket:

1. `SetStorageClass COLDLINE` after `age_days: 90` — cheapen at-rest cost
   for data the #1 age filter has long stopped reading.
2. `Delete` after `age_days: 365` — well beyond the 30-day pull TTL, so by
   the time it fires the data is provably unused.

`ra create` applies these idempotently after `EnsureBucket` (GCS replaces
the whole lifecycle config each call). The mechanism is generic in core —
any plugin can declare `lifecycle:` on a bucket; no claude-sync name is
hard-coded. See the plugin README for the equivalent
`gcloud storage buckets update --lifecycle-file` runbook (the fallback if
you cannot re-run `ra create`).

**Optional, not implemented**: a periodic "archive idle namespaces" sweep
that moves namespaces idle > N days under a `workstations-archive/` prefix
so the boot `gcloud storage ls` listing stays small even before lifecycle
deletion fires. With #1 already capping how many namespaces are *read*,
this is only worth doing if the raw `ls` of `workstations/` itself becomes
slow (hundreds+ of entries). It would be a separate cron/`ExecStartPost`
job; the pull would also need to list `workstations-archive/` to keep
merging recently-archived data within the TTL.

---

## v2 improvements

- **Real-time sync** via inotify + GCS Go client.
- **Pre-shutdown hook**: systemd `ExecStop=`.
- **CLI surface**: `ra claude sync push|pull|status|diff|reset|gc`.
- **Conflict surfacing**: detect mtime-tied edits and prompt to choose.
- **Team sharing**: `shared/` prefix + per-user IAM grants.
- **Encryption**: CMEK on the bucket; client-side encryption for high-sensitivity workstations.
- **Selective sync**: per-project include/exclude patterns.
- **Backend abstraction**: swap GCS for S3/R2 via a `backend:` field.
- **Versioning**: enable GCS object versioning + `ra claude sync history <file>`.
- **Compaction**: nightly roll-up of `history.jsonl` into `_consolidated/`.

---

## Verification

Manual end-to-end after the v1 build is in place:

1. `make build && make install`.
2. `ra plugin install ./plugins/claude-sync` (level: user).
3. `ra create ws-alpha` — confirm bucket `ra-<project>-<hash>` appears in GCS console, and the workstation SA has `objectAdmin` on it.
4. `ra connect ws-alpha`, run `claude -p "hello"`, observe `~/.claude/projects/<cwd>/<sessionId>.jsonl` filling.
5. Wait ~70s, run `gcloud storage ls gs://<bucket>/claude-sync/workstations/ws-alpha/projects/...` — confirm the JSONL is there.
6. `ra create ws-bravo` from another terminal. Once it boots, `ra connect ws-bravo` and confirm `~/.claude/projects/<cwd>/<sessionId>.jsonl` from ws-alpha is present locally.
7. Run claude on ws-bravo in the same project. Wait 70s. On ws-alpha, restart the workstation (or manually re-run `/etc/workstation-startup.d/270_claude-sync-pull.sh`); confirm ws-bravo's session JSONL is now present.
8. `go test ./internal/gcs/... ./internal/scaffold/...` for the new helper + scaffold tests.
9. (Optional) Validate filter: write `/home/user/.claude/shell-snapshots/test.sh`, wait 70s, confirm it does NOT appear in GCS.
