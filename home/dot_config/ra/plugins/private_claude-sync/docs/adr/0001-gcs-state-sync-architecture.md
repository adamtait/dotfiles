# 1. GCS-backed cross-workstation state sync

- Status: accepted
- Applies to: `claude-sync` plugin

## Context

Claude Code writes local state under `/home/user/.claude/` and `/home/user/.claude.json`: session
transcripts (`projects/<cwd>/<sessionId>.jsonl`), the global config (`.claude.json`), a global
command log (`history.jsonl`), CLAUDE.md memory files, todos, plans, etc. On a Cloud Workstation
that state lives only on the boot disk — it survives stop/start but is destroyed when the workstation
is deleted, and never crosses between workstations. A user who creates a new workstation loses all
prior session context.

We want to share session history, config, and memory across a user's workstations, given that:
workstations often run in parallel and write concurrently (so we need per-workstation namespacing to
avoid corruption), and we want to reuse the existing "shell out to gcloud" pattern rather than stand
up new VPC/NFS infrastructure.

## Decision

A new `claude-sync` plugin (separate from `claude`, depending on it) with a pull-on-boot +
continuous-push design over a per-user GCS bucket:

- **Pull once per boot** (`workstation-startup.d/270_claude-sync-pull.sh`, after the claude plugin's
  setup): merge every workstation's namespace from the bucket down into the local `~/.claude/`.
- **Push continuously** (`workstation-startup.d/271_claude-sync-daemon.sh` launches a background loop):
  every `interval_seconds` (default 60), `gcloud storage rsync` a **filtered allowlist** of
  `~/.claude/` up to `gs://<bucket>/claude-sync/workstations/<this-ws-id>/`.
- **No shutdown hook in v1.** Worst-case loss is ≤ one interval of state; session JSONLs are
  append-only, so the next pull picks them up on restart.

**Per-workstation namespacing** in GCS (`workstations/<ws-id>/`) means concurrent writes from
different workstations never touch the same object; merging happens on read.

### Bucket provisioning is a generic CLI mechanism, not claude-sync-specific

`ra create` provisions the bucket (idempotently) and grants the workstation service account
`roles/storage.objectAdmin` on it. Crucially, **no plugin name is hard-coded in the CLI**: the plugin
declares its needs in `plugin.yaml` via a generic `buckets:` field (name template, optional override
field, IAM role, and the env var to expose the resolved name under), and the CLI walks that
declaration for every enabled plugin. This mirrors the existing pattern for plugin-declared
`auth_providers[].iam_roles:` and `port-for-tunnel:` — policy in `plugin.yaml`, mechanism in core.
The workstation runtime contract is just the injected `RA_PLUGIN_CLAUDE_SYNC_BUCKET` env var.

```yaml
buckets:
  - name: state
    name_template: "ra-{project_id}-{user_email_hash}"
    override_field: bucket_name_override
    role: roles/storage.objectAdmin
    env_var: RA_PLUGIN_CLAUDE_SYNC_BUCKET
```

### Push allowlist, not exclude-regex

The push loop iterates a fixed allowlist — `projects/`, `todos/`, `plans/`, `session-env/`,
`history.jsonl`, `.claude.json` — and pushes each explicitly, rather than one whole-tree rsync with
`--exclude` patterns for the volatile paths (`shell-snapshots/`, `sessions/`, `cache/`, `backups/`,
`statsig/`, `plugins/`). `gcloud storage rsync`'s `--exclude` is a single regex that is awkward to
compose across many directories; the allowlist makes intent obvious ("sync exactly these six
things"). Tradeoff: a new first-level entry Claude Code adds in a future version is not picked up
automatically — but the set worth syncing is small and known, and new entries want vetting anyway
(they may be machine-specific or contain secrets).

### Workstation id resolved at runtime, not injected at create time

The boot scripts compute `WS_ID` themselves rather than receiving it as an env var. `--container-env`
is set on the workstation *config*, which is shared across every instance of that config; a single
static value there would give every instance the same id and defeat namespacing. (See ADR 0002 for
the hardened resolver and ADR 0004 for the later stable, deterministic namespace.)

### Daemon runs as root; systemd unit is emitted inline

The push loop runs as root: it can read `~/.claude/` regardless of file modes, and gcloud's ADC comes
from the metadata server (reachable by any uid), so root adds no attack surface beyond what the core
boot step already needs. The systemd unit and loop script are written to disk via heredocs from
`271_*.sh` rather than shipped as separate asset files, because the plugin build only packages the
`Dockerfile.d/`, `workstation-startup.d/`, and `profile.d/` subdirectories — an `assets/` dir would
be silently ignored. The unit is ~12 lines; if more units are added later this should be revisited.

## Consequences

- The next plugin that needs a bucket adds zero lines to the CLI — it just declares `buckets:`.
- Storage is ~10–50 MB per workstation (~100 MB/user), negligible at GCS pricing; steady-state
  bandwidth is small because rsync only reships changed files.
- Session JSONLs may contain shell/tool output and pasted content. The bucket inherits project-level
  encryption and is IAM-locked to the workstation SA + project owners; this risk is documented in the
  plugin README, with CMEK recommended for a future version.
- The pull/merge semantics for the shared append-only files (`history.jsonl`, `.claude.json`) needed
  more care than a plain rsync — see ADR 0002.
