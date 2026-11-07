# claude-sync

**Your Claude Code history, todos, plans, and memory follow you to every workstation — not your laptop.**

A [`remote-agent`](https://github.com/adamtait/remote-agent) plugin that syncs `~/.claude/` across
cloud workstations sharing a `(GCP project, git user email)` pair. Each workstation pushes its state
to a private GCS bucket on a short loop; on boot, every other workstation's state is merged back in.

```console
$ ra plugin install https://github.com/adamtait/ra-plugins claude-sync
Enable claude-sync? [y/N] y
Seconds between push cycles of ~/.claude/ to GCS [60]:
Override the auto-derived GCS bucket name (empty = use ra-<project>-<hash>):
# → next `ra create` provisions gs://ra-<project>-<hash> and grants the
#   workstation service account objectAdmin on it

# Workstation A, Monday: you run a Claude Code session.
# Workstation B, Tuesday — the boot pull merges A's state into ~/.claude/:
$ claude          # up-arrow replays A's prompts; A's transcripts, todos,
                  # and plans are already under ~/.claude/projects
```

## Why claude-sync?

- **State outlives any single workstation.** Session transcripts, todos, plans, per-session env, the
  global command log, and per-project `CLAUDE.md` memory land in `~/.claude/` on every workstation —
  hand a session off between machines, or review usage across all of them from one place.
- **An allowlist, not a blob.** Exactly the six paths below are pushed each cycle; PIDs, caches, the
  npm plugin dir, and shell snapshots are excluded by design, so nothing machine-local leaks.
- **No write contention.** Each workstation writes only to its own namespace prefix and the pull
  *union-merges* every other namespace, so parallel machines never clobber each other's history.
- **No reboot needed.** The pull runs at boot and again every `pull_interval_seconds` (default 10
  minutes), so a session started on another workstation shows up here while you work.
- **Bounded growth, logged — not silently capped.** The pull merges only namespaces pushed
  within `pull_max_age_days` and caps at the `pull_max_namespaces` most recent; GCS lifecycle rules
  demote data to Coldline at 90 days and delete at 365. Every exclusion is logged as `skip <ns>: <reason>`.

## What gets synced

Pushed every cycle (allowlist):

- `projects/` — session transcripts (UUID-named JSONLs) and per-project memory directories.
- `todos/`, `plans/`, `session-env/` — UUID-keyed todo, plan, and per-session env state.
- `history.jsonl` — global command log. **Union-merged** on boot: it is append-only and each
  workstation holds only a partial view, so copies are merged, never overwritten.
- `.claude.json` — global config. The **local** file wins for all machine-local fields (auth,
  onboarding, counters); only per-project `history` arrays are unioned in from other workstations.

Excluded (deliberately local): `shell-snapshots/`, `sessions/`, `plugins/`, `cache/`, `backups/`, `statsig/`.

## Install

```bash
ra plugin install https://github.com/adamtait/ra-plugins claude-sync
```

Enable it (default `false` → say yes) and accept the defaults unless you have a specific bucket name
to reuse. The next `ra create` will, idempotently:

1. Create the bucket `ra-<project>-<sha1(git-user-email)[:8]>` (or your `bucket_name_override`).
2. Bind `roles/storage.objectAdmin` on it to the workstation service account.
3. Inject `RA_PLUGIN_CLAUDE_SYNC_BUCKET`. Each workstation pushes to its own namespace, named after
   the workstation instance and never reused (a rebuild starts a new one; its history comes back
   through the pull, [ADR 0007](docs/adr/0007-unique-workstation-identifiers.md)).
4. Apply the bucket's GCS Object Lifecycle rules (see [Storage lifecycle](#storage-lifecycle)).

claude-sync syncs files created by the [`claude`](../claude/) plugin (or a manual Claude Code
install). With neither present, pushes are no-ops — install both together.

## Architecture

The bucket name omits "claude" so other plugins can share it under their own prefixes. See the
[ADRs](./docs/adr/) for the full design.

```
Workstation A ──┐                              ┌── Workstation C
                ├── gs://ra-<proj>-<hash>/  ───┤
Workstation B ──┘     claude-sync/             └── Workstation D
                        workstations/<ws-id>/...
                        _manifest/<ws-id>.json
```

- Each workstation pushes only to `claude-sync/workstations/<ws-id>/` → no concurrent-write contention.
- The pull merges every other namespace into `~/.claude/`: directory trees via `cp -an` (newer local
  files win), `history.jsonl` via union-merge, `.claude.json` via local-base + per-project history union.
  The periodic pull does all of this except `.claude.json`, which only the boot pull touches.
- `_manifest/<ws-id>.json` carries `last_push_at_epoch`, advanced only on a fully successful push cycle,
  and drives the boot pull's recency ranking.

## Configuration

| Field                  | Type   | Default                     | Notes                                                                 |
|------------------------|--------|-----------------------------|-----------------------------------------------------------------------|
| `enabled`              | bool   | `false`                     | Master switch.                                                        |
| `interval_seconds`     | int    | `60`                        | Push cadence. Below 10 amortizes the gcloud cold-start cost poorly.   |
| `bucket_name_override` | string | (derive `ra-<proj>-<hash>`) | Must be valid GCS naming (lowercase, 3–63 chars, no underscores).     |
| `sync_all_projects`    | bool   | `false`                     | User-level install only. Points every project's workstations at one shared bucket. |
| `pull_max_age_days`    | int    | `30`                        | Boot pull merges only namespaces pushed within N days. `0` = cap-only. |
| `pull_max_namespaces`  | int    | `15`                        | Boot pull caps the merge to the N most-recently-pushed namespaces.    |
| `pull_interval_seconds`| int    | `600`                       | Seconds between periodic pulls. `0` = boot pull only. Same age/cap filter. |

## Operations

- **Logs**: `/var/log/ra-claude-sync.log` on each workstation.
- **Process check**: `pgrep -af ra-claude-sync-loop` — works under both launch paths. Cloud
  Workstations run `entrypoint.sh` as PID 1 (not systemd), so the `setsid` fallback is what runs in
  production; `systemctl status` reporting "Failed to connect to bus" there is expected, not the bug.
- **Restart the loop**: re-run `/etc/workstation-startup.d/271_claude-sync-daemon.sh` as root
  (idempotent — SIGTERM the PID in `/run/ra-claude-sync.pid`, then relaunch).
- **Manual push / pull**: run `/usr/local/bin/ra-claude-sync-loop.sh` (Ctrl-C to stop), or re-run
  `/etc/workstation-startup.d/270_claude-sync-pull.sh` as root.
- **Inspect a namespace**: `gcloud storage ls gs://<bucket>/claude-sync/workstations/<ws-id>/projects/`.

### Storage lifecycle

The bucket declares two GCS lifecycle rules in `plugin.yaml`, both scoped to the `claude-sync/`
prefix so they never touch other plugins' objects: **Coldline after 90 days** and **Delete after
365 days** (well beyond the 30-day boot-pull TTL, so anything deleted is provably unused). `ra create`
applies them idempotently; tune the horizons by editing `buckets[].lifecycle` and re-running.

To reclaim a specific dead workstation's namespace immediately:

```bash
gcloud storage rm -r "gs://<bucket>/claude-sync/workstations/<old-ws-id>/"
gcloud storage rm "gs://<bucket>/claude-sync/_manifest/<old-ws-id>.json"
```

## ⚠️ Security: transcripts contain whatever Claude saw

Session JSONLs under `projects/<cwd>/` capture full conversational history — shell output, tool
results, pasted content — and flow verbatim into the GCS bucket.

- The bucket is IAM-locked to the workstation SA + project owners, with Google-managed encryption.
- **Do not enable this plugin on workstations handling data you would not also store in a private
  GCS bucket.** For high-sensitivity environments, run `ra` without it (or wait for v2's CMEK /
  client-side encryption).

## Caveats

- Up to `interval_seconds` of state can be lost if a workstation is killed between push cycles.
- The periodic pull brings in *new* sessions. A transcript that already exists locally is never
  updated (`cp -an`), so a session you continue on another machine stays at the older copy here.
  A transcript you deleted locally can reappear within one `pull_interval_seconds`.
- `gcloud storage rsync` re-uploads whole files on any byte change — a 50 MB session JSONL re-uploads
  fully each cycle. v2 plans a delta-append approach.
- Parallel `CLAUDE.md` memory edits on two workstations resolve by last-merge-order on pull; no
  conflict UI yet.
- `.claude.json` merge can create a project entry holding only a `history` array for projects you've
  never opened here (so up-arrow history works when you first `cd` in); Claude fills the rest with defaults.

## License

[MIT](../LICENSE) © Adam Tait
