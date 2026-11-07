# 4. Load reduction: filtered pull, stable namespace, bucket lifecycle

- Status: accepted (2026-06)
- Applies to: `claude-sync` plugin (+ the generic bucket-lifecycle mechanism in `ra` core)

## Context

By mid-2026 the sync bucket held ~36 namespaces / ~2.2 GB (~85 MB each) and was growing without bound.
Two causes: `WS_ID` defaulted to the GCE instance name, which is fresh on every `ra create`, so a
workstation rebuilt N times left N namespaces; and nothing ever pruned. Detaching the boot pull had
already stopped the pull from breaking boot, but the background pull cost still scaled with the
dead-namespace count.

## Decision

Three changes bound the cost. All defaults are conservative — the 30-day pull window sits comfortably
inside the 90-day Coldline / 365-day Delete horizons — so no live data is at risk.

### #1 — Recency/age-filtered, capped boot pull (`270_*.sh`)

Before staging, rank every candidate namespace by its manifest's `last_push_at_epoch` and merge only
those that are **both** newer than `pull_max_age_days` **and** within the `pull_max_namespaces`
most-recent. Our own namespace is always skipped (push-only). A missing/invalid manifest is treated as
epoch 0 (oldest → dropped first) so undated namespaces never crowd out a dated active one. Every
exclusion is logged (own-namespace, beyond-TTL, no-manifest, beyond-cap) — nothing is silently
truncated. The union-merge model (ADR 0002) is unchanged; it just operates over the filtered set.

| Config key            | Env var                                       | Default | Meaning |
|-----------------------|-----------------------------------------------|---------|---------|
| `pull_max_age_days`   | `RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_AGE_DAYS`     | `30`    | Only merge namespaces pushed within this many days. `0` = no age limit (cap-only). |
| `pull_max_namespaces` | `RA_PLUGIN_CLAUDE_SYNC_PULL_MAX_NAMESPACES`   | `15`    | Cap the merge to the N most-recently-pushed namespaces. |

### #3 — Stable per-user namespace (`ra create` + both scripts)

> **Superseded by [ADR 0007](./0007-unique-workstation-identifiers.md).** The stable id is per
> workstation *config*, so workstations on one config shared a namespace and overwrote each other.
> #1 and #4 below still stand.

`ra create` injects `RA_PLUGIN_CLAUDE_SYNC_NAMESPACE`, a deterministic id derived from
`(gcloud project, workstation config name, git user email)` — lowercased, restricted to `[a-z0-9_-]`,
slash-free, and suffixed with an 8-hex digest of the raw triple so distinct triples never collide.
Injection is gated on the plugin being enabled. Both scripts prefer this env var over `resolve_ws_id()`
and fall back to the metadata/cache/hostname chain (ADR 0002) only when it is unset. A rebuilt
workstation now reuses one namespace instead of minting a new one.

### #4 — Bucket Object Lifecycle Management (`plugin.yaml` + `ra` core)

The `state` bucket declares two lifecycle rules via a generic `buckets[].lifecycle:` field, both scoped
to the `claude-sync/` prefix (`matchesPrefix`) so they never touch objects other plugins write into the
shared bucket:

1. `SetStorageClass COLDLINE` after 90 days — cheapen at-rest cost for data the #1 age filter has long
   stopped reading.
2. `Delete` after 365 days — well beyond the 30-day pull TTL, so the data is provably unused by then.

`ra create` applies these idempotently (GCS replaces the whole lifecycle config each call). The
mechanism is generic in core — any plugin may declare `lifecycle:` on a bucket; no claude-sync name is
hard-coded. The plugin README documents the equivalent `gcloud storage buckets update --lifecycle-file`
runbook as a fallback.

## Consequences

- **#1 is recency-biased:** a namespace untouched for > `pull_max_age_days` stops being merged even if
  it holds unique history. It remains in the bucket (read-only) until lifecycle Delete fires and is
  recoverable by raising `pull_max_age_days`.
- **#3's id is a hash of the config name:** renaming the workstation config forks the namespace once
  (old data still merged via #1 until it ages out). No data is moved or deleted when #3 is enabled — old
  UUID namespaces simply stop receiving writes, age out of the read window, then get reclaimed by #4.
- **#4 replaces the bucket's entire lifecycle config on each `ra create`**, so a rule added by hand
  out-of-band would be overwritten — all rules must be declared in `plugin.yaml`.
- **Not implemented (documented only):** a periodic "move idle namespaces under a
  `workstations-archive/` prefix" sweep. Since #1 already caps how many namespaces are *read*, this is
  only worth adding if the raw `ls` of `workstations/` itself becomes slow (hundreds of entries).
