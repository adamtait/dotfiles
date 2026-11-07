# 5. Bucket naming, shared-bucket prefixing, and cross-project sync

- Status: accepted (2026-05-22)
- Applies to: `claude-sync` plugin

## Context

The sync bucket was originally named and laid out as if `claude-sync` exclusively owned it. As the
bucket became a shared resource that other plugins could also write into, both the name and the object
layout needed to stop implying sole ownership, and users needed a way to point several GCP projects at
a single bucket.

## Decision

- **Generic bucket name.** The name template changed from `ra-claude-{project_id}-{user_email_hash}` to
  `ra-{project_id}-{user_email_hash}`. Embedding "claude" implied exclusive ownership and would mislead
  if, say, a `github-sync` plugin also wrote to it. The name is derived as
  `ra-<project-id>-<sha1(user-email)[:8]>` so it is globally unique (GCS bucket names are global) and
  does not leak the raw email; it can be overridden with `bucket_name_override`.
- **Plugin-namespaced object paths.** All objects the plugin writes are prefixed with `claude-sync/`
  (e.g. `gs://<bucket>/claude-sync/workstations/<ws-id>/...`, `.../claude-sync/_manifest/...`). Without
  a prefix, the top-level bucket namespace is unpartitioned and a second plugin writing `workstations/`
  or `_manifest/` objects would collide. The prefix is an unambiguous namespace in the GCS console and
  in gcloud commands.
- **`sync_all_projects` config option** (bool, default false). At a **user-level** install, when set and
  `bucket_name_override` is not already provided, the install resolves the user's home-project bucket
  (`ra-{user_project_id}-{user_email_hash}`) from the **user-level** config and stores it as the
  override, so workstations across all of that user's GCP projects sync to one bucket. The user config
  is read directly (not the merged config) so the user's home project is used even when `ra plugin
  install` runs from a project directory that overrides `project_id`. At **project-level** install the
  option is stored but does not auto-derive an override (project installs are expected to be
  project-specific); the user can still set `bucket_name_override` manually.

## Consequences

- **Migration cost for the rename and the prefix.** Deployments that already provisioned a
  `ra-claude-*` bucket (or wrote objects at the old un-prefixed paths) are not auto-migrated: the next
  `ra create` uses the new bucket/paths, and old data is orphaned unless the user copies it or sets
  `bucket_name_override: ra-claude-<proj>-<hash>` to keep using the old bucket. Documented rather than
  coded as a migration because the plugin is young and production deployments are few.
- **Internal artifact names keep "claude-sync".** The systemd service name, the log-file path
  (`/var/log/ra-claude-sync.log`), and the loop binary all still include `claude-sync` — these are
  per-workstation, plugin-scoped artifacts, not global GCS identifiers, so keeping them plugin-specific
  is correct.
- **`sync_all_projects` uses the user-level email.** If the user's email differs between user and
  project configs, the shared bucket name uses the user-level email. This is intended — we want a
  stable, user-anchored identifier — but it means the `sync_all_projects` bucket can differ from the
  per-project bucket in the same GCP project if the emails diverge.
- **Cross-project IAM works without special-casing.** When `sync_all_projects` points project B's
  workstations at a bucket owned by project A, the existence check succeeds globally (GCS names are
  global), bucket creation is skipped, and project B's workstation SA is granted `objectAdmin` on the
  bucket — a bucket-level grant that works cross-project. No core changes were needed for this.
