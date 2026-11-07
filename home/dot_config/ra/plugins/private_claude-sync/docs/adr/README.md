# claude-sync — Architecture Decision Records

Decisions specific to the `claude-sync` plugin. See the
[top-level ADR index](../../../docs/adr/README.md) for cross-cutting decisions.

- [0001 — GCS-backed cross-workstation state sync](./0001-gcs-state-sync-architecture.md)
- [0002 — Merge-based sync for append-only / shared files](./0002-merge-based-sync.md)
- [0003 — Daemon launch: setsid fallback when systemd is not PID 1](./0003-daemon-launch-setsid-fallback.md)
- [0004 — Load reduction: filtered pull, stable namespace, bucket lifecycle](./0004-load-reduction.md)
- [0005 — Bucket naming, shared-bucket prefixing, and cross-project sync](./0005-bucket-naming-and-namespacing.md)
- [0006 — Rebuild restore, merge-on-push, and shared-namespace safety](./0006-rebuild-restore-and-merge-on-push.md) *(superseded by 0007)*
- [0007 — Workstation identifiers are unique and never reused](./0007-unique-workstation-identifiers.md)
- [0008 — Periodic pull](./0008-periodic-pull.md)
