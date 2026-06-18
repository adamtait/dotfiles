# `claude-sync` plugin

Syncs `~/.claude/` across cloud workstations belonging to the same
(GCP project, git user email) pair. Backed by a per-user GCS bucket and
a 60-second push loop running on each workstation (launched via systemd
when PID 1 is systemd, otherwise via `setsid` — see Operations below).

## What gets synced

Allowlist — exactly these paths are pushed every cycle:

- `projects/` — Claude Code session transcripts (UUID-named JSONLs) and
  per-project memory directories.
- `todos/` — UUID-keyed todo state.
- `plans/` — UUID-keyed plan documents.
- `session-env/` — per-session environment captures.
- `history.jsonl` — global command log. **Union-merged** across
  workstations on boot pull (it is an append-only log of session
  prompts; each workstation holds only a partial view, so copies are
  merged, never overwritten).
- `.claude.json` — global config / onboarding state. The **local** file
  is authoritative for all machine-local fields (auth, onboarding,
  counters); only per-project `history` arrays are unioned in from other
  workstations.

Excluded (deliberately — local to each workstation):

- `shell-snapshots/`, `sessions/` (PIDs), `plugins/` (npm cache),
  `cache/`, `backups/`, `statsig/`.

## Architecture

See [`SPEC.md`](./SPEC.md) for the full design and
[`implementation-notes.md`](./implementation-notes.md) for the running
log of decisions and deviations.

```
Workstation A ──┐                              ┌── Workstation C
                ├── gs://ra-<proj>-<hash>/  ───┤
Workstation B ──┘     claude-sync/             └── Workstation D
                        workstations/<ws-id>/...
                        _manifest/<ws-id>.json
```

- Each workstation pushes only to its own `claude-sync/workstations/<ws-id>/`
  prefix → no concurrent-write contention.
- Boot pull merges every other namespace into `~/.claude/`:
  `projects/`, `todos/`, `plans/`, `session-env/` via `cp -an` (newer
  local files win); `history.jsonl` via union-merge; `.claude.json` via
  local-base + per-project history union.
- `claude-sync/_manifest/<ws-id>.json` carries `last_push_at_epoch`,
  advanced only on a fully-successful push cycle. It is retained for
  observability and future incremental pull; the boot pull no longer
  needs it to choose a namespace (the union merge is order-independent).
- The bucket name (`ra-<project>-<hash>`) omits "claude" so future plugins
  can share it under their own prefixes.

## Installation

```bash
ra plugin install ./plugins/claude-sync
# Prompted for: enabled (default false → say yes), interval_seconds
# (default 60), bucket_name_override (leave empty unless you have a
# specific GCS bucket name to use).
```

The next `ra create` will:

1. Idempotently create the bucket
   `ra-<gcp-project>-<sha1(git-user-email)[:8]>` (or whatever
   you put in `bucket_name_override`).
2. Bind `roles/storage.objectAdmin` on it to the workstation service
   account.
3. Inject `RA_PLUGIN_CLAUDE_SYNC_BUCKET` into the workstations
   container env so the boot scripts know where to push/pull.
4. Inject `RA_PLUGIN_CLAUDE_SYNC_NAMESPACE` — a stable namespace id (see
   "Stable namespace" below) — so a rebuilt workstation reuses one
   namespace instead of spawning a new one each create.
5. Apply the bucket's declared GCS Object Lifecycle rules (see "Storage
   lifecycle" below), idempotently.

## Configuration

| Field                  | Type   | Default                         | Notes                                                                |
|------------------------|--------|---------------------------------|----------------------------------------------------------------------|
| `enabled`              | bool   | `false`                         | Master switch.                                                       |
| `interval_seconds`     | int    | `60`                            | Push cadence. Below 10 starts to amortize the gcloud cold-start cost poorly. |
| `bucket_name_override` | string | (derive `ra-<proj>-<hash>`)     | Must match GCS bucket naming (lowercase, 3–63 chars, no underscores). |
| `sync_all_projects`    | bool   | `false`                         | User-level install only. Auto-sets `bucket_name_override` to the user's default GCP project bucket so all workstations across all GCP projects sync to one place. |
| `pull_max_age_days`    | int    | `30`                            | Boot pull only merges namespaces pushed within this many days. `0` = no age limit (cap only). Surfaced to `270_*.sh` as `RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_AGE_DAYS`. |
| `pull_max_namespaces`  | int    | `15`                            | Boot pull caps the merge to the N most-recently-pushed namespaces. Surfaced as `RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_NAMESPACES`. |

### Boot-pull namespace selection (load reduction)

Without GC, every past `ra create` left a namespace behind and the boot
pull merged all of them — gigabytes across dozens of dead namespaces on
every boot. The pull now ranks namespaces by their manifest's
`last_push_at_epoch` and merges only those within `pull_max_age_days`
**and** within the top `pull_max_namespaces` by recency. Namespaces with
no/invalid manifest are treated as the oldest and drop off first. Every
exclusion is logged to `/var/log/ra-claude-sync.log` as
`skip <ns>: <reason>` — there is no silent truncation.

### Stable namespace (load reduction)

`WS_ID` used to default to the GCE instance name — a fresh UUID on every
`ra create` — so a rebuilt workstation spawned a brand-new ~85 MB
namespace each time. When `ra create` injects
`RA_PLUGIN_CLAUDE_SYNC_NAMESPACE` (a stable id derived from GCP project +
workstation `config_name` + git user email), both `270_*.sh` and
`271_*.sh` prefer it, so a rebuilt workstation reuses **one** namespace.
The metadata / cache / `hostname` chain remains the fallback when the env
var is unset. Existing UUID namespaces stay as read-only history and are
merged from until they age out of the pull window above; meanwhile new
writes go only to the stable namespace.

## Operations

- **Logs**: `/var/log/ra-claude-sync.log` on each workstation.
- **Process check**: `pgrep -af ra-claude-sync-loop` — works under both
  the systemd and `setsid` launch paths. Cloud Workstations' default base
  image runs `entrypoint.sh` as PID 1 (not systemd), so the `setsid`
  fallback is what runs in production; on those workstations
  `systemctl status ra-claude-sync.service` reports "Failed to connect
  to bus" and is not the right diagnostic.
- **Restart the loop**: re-run `/etc/workstation-startup.d/271_claude-sync-daemon.sh`
  as root. It is idempotent — under the `setsid` path it sends SIGTERM
  to the loop PID recorded in `/run/ra-claude-sync.pid` (5s grace before
  SIGKILL) and then relaunches; under the systemd path it `systemctl
  restart`s the unit.
- **Manual push**: `/usr/local/bin/ra-claude-sync-loop.sh` (just run
  the loop body interactively — Ctrl-C to stop).
- **Manual pull**: re-run `/etc/workstation-startup.d/270_claude-sync-pull.sh`
  as root.
- **Inspect a namespace**:
  `gcloud storage ls gs://<bucket>/claude-sync/workstations/<ws-id>/projects/`.

### Storage lifecycle (automatic GC)

The bucket declares two GCS Object Lifecycle Management rules in
`plugin.yaml`, both scoped to the `claude-sync/` prefix so they never
touch objects other plugins write into the shared bucket:

1. **Coldline after 90 days** — old, no-longer-pulled data is demoted to
   the cheaper Coldline storage class.
2. **Delete after 365 days** — well beyond the 30-day boot-pull TTL, so
   anything this deletes is provably unused.

`ra create` applies them idempotently (GCS replaces the whole lifecycle
config each call). Tune the horizons by editing `buckets[].lifecycle` in
`plugin.yaml` and re-running `ra create`.

If you cannot re-run `ra create`, apply the same policy by hand
(`--lifecycle-file` replaces the bucket's entire lifecycle config):

```bash
cat > /tmp/claude-sync-lifecycle.json <<'JSON'
{
  "rule": [
    {
      "action": { "type": "SetStorageClass", "storageClass": "COLDLINE" },
      "condition": { "age": 90, "matchesPrefix": ["claude-sync/"] }
    },
    {
      "action": { "type": "Delete" },
      "condition": { "age": 365, "matchesPrefix": ["claude-sync/"] }
    }
  ]
}
JSON
gcloud storage buckets update "gs://<bucket>" \
  --lifecycle-file=/tmp/claude-sync-lifecycle.json
# Inspect the active policy:
gcloud storage buckets describe "gs://<bucket>" --format='value(lifecycle_config)'
```

### Cleaning up stale namespaces from deleted workstations

The lifecycle rule above eventually deletes stale data automatically, and
the boot pull's age/cap filter (see "Boot-pull namespace selection")
stops *reading* it well before that. To reclaim a specific dead
workstation's namespace immediately:

```bash
gcloud storage rm -r "gs://<bucket>/claude-sync/workstations/<old-ws-id>/"
gcloud storage rm "gs://<bucket>/claude-sync/_manifest/<old-ws-id>.json"
```

> **Optional, not implemented**: an "archive idle namespaces" sweep that
> moves namespaces idle > N days under a `workstations-archive/` prefix to
> keep the boot `gcloud storage ls workstations/` listing small. With the
> pull already capping how many namespaces it *reads*, this is only worth
> adding if the raw `ls` itself slows down (hundreds+ of entries). See
> SPEC.md "Load reduction" for the design sketch.

## ⚠️ Security: transcripts contain whatever Claude saw

Session JSONLs under `projects/<cwd>/` capture full conversational
history including shell output, tool results, and pasted content. Those
flow verbatim into the GCS bucket.

- The bucket is locked down by IAM: only the workstation SA + project
  owners can read it.
- The bucket uses Google-managed default encryption.
- **Do not enable this plugin on workstations that handle data you
  would not also store in a private GCS bucket.**
- For high-sensitivity environments, wait for v2's CMEK / client-side
  encryption support, or run `ra` without the plugin.

## Dependency on the `claude` plugin

claude-sync syncs files under `~/.claude/`. Those files are created by
the `claude` plugin (or by claude-code itself when installed manually).
If neither is present on a workstation, `claude-sync`'s push cycles are
all no-ops — the pull is also a no-op the first time, but on subsequent
boots it will populate `~/.claude/` with state pushed from other
workstations. Recommended: install both plugins together.

## Caveats

- ≤`interval_seconds` of state can be lost if a workstation is killed
  between push cycles (the next boot pull picks up the union of whatever
  was last persisted across all namespaces).
- `gcloud storage rsync` re-uploads whole files on any byte change. A
  long-running session whose JSONL grows to 50 MB will re-upload all
  50 MB once per cycle. v2 plans a delta-append approach.
- Per-workstation namespacing means CLAUDE.md memory edits made on
  workstation A and workstation B in parallel use last-merge-order on
  pull; no conflict UI yet.
- Boot pull cost grows with namespace count: it now folds in every
  namespace's `history.jsonl` and `.claude.json`. Fine at tens of
  workstations; v2 will skip unchanged namespaces via the manifest.
- `.claude.json` merge can create a project entry containing only a
  `history` array for projects you have never opened on this workstation
  (so prior up-arrow history is available when you first `cd` there).
  claude fills the remaining project fields with defaults.
