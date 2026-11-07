# 7. Workstation identifiers are unique and never reused

- Status: accepted (2026-11-06); implemented — see [Implementation status](#implementation-status).
- Supersedes: [ADR 0004](./0004-load-reduction.md) #3 (stable per-user namespace) and
  [ADR 0006](./0006-rebuild-restore-and-merge-on-push.md) (whose implementation, PR #34, is reverted)
- Applies to: `claude-sync` plugin

## Context

ADR 0004 #3 made the namespace stable across rebuilds, derived from
`(gcloud project, workstation config name, git user email)`, so that rebuilding a workstation stopped
leaving one dead ~85 MB namespace behind per `ra create`.

That id is injected through the workstation **config's** container env, so it is per config. Every
workstation built from one config gets the same value and writes to the same namespace. In practice
two live workstations erased each other's `history.jsonl` every ~5 minutes (the incident in ADR 0006's
context). `ra` core cannot make the id per workstation: a first attempt that added the workstation name
to the id (remote-agent#158) was reverted, because config-level env cannot differ per workstation and the
name `ra create` has at that point is only a base that gets a random suffix later.

ADR 0006 / PR #34 responded by making a shared namespace safe: merge-on-push with generation
preconditions, a writer WARN, a once-per-disk restore of the own namespace, and a pull-state handshake.
That machinery exists only because one identifier is shared by several writers. It treats the symptom of
reusing an identifier, not the reuse.

A stable identifier is also not what carries history between machines. The pull already does that: at
boot it merges every recent namespace into the local `~/.claude` (transcripts copied in when missing,
`history.jsonl` union-merged, per-project `.claude.json` history unioned). Because each workstation
re-pushes what it pulled, history spreads transitively to every machine that comes up afterwards.

## Decision

1. **An identifier names exactly one workstation instance and is never reused.** Not across rebuilds, not
   across workstations created from the same config, not after a workstation is deleted. Each namespace
   therefore has exactly one writer for its whole life.
2. **No identifier is derived from shareable inputs.** Config name, project, user email, and anything else
   that two workstations can have in common are not used, so no stable id is injected or consumed.
3. **The identifier is the GCE instance name** (`workstations-<uuid>` on Cloud Workstations), as it was
   before ADR 0004 #3, resolved by `resolve_ws_id()` with its existing metadata → cache → hostname
   fallback (ADR 0002).
4. **Continuity across rebuilds comes from the cross-namespace pull**, not from reusing a namespace. A
   rebuilt workstation's predecessor is just another namespace to pull.
5. **The shared-namespace machinery is not needed and is not kept:** no merge-on-push, no writer-collision
   guard or WARN, no own-namespace restore marker. With one writer per namespace, a plain overwrite of
   `history.jsonl` / `claude.json` is correct, and the pull skips its own namespace (ADR 0004 #1), since
   everything in it was written from this disk.

### Rejected

- **Stable per-config / per-user id** (ADR 0004 #3). Shared by construction; caused the incident.
- **Make a shared namespace safe** (ADR 0006). Works, but permanently carries the merge-on-push and
  guard machinery to paper over an identifier that should not be shared.
- **`<stable-id>--<hostname>`** (PR #35). Deliberately reuses a namespace when a workstation is rebuilt under
  the same name, which this rule forbids, and it is unverified whether the hostname differs across
  `ra create` runs.

## Consequences

- **Namespaces accumulate again**, one per `ra create`. This is the growth ADR 0004 #3 was written to
  stop. It stays bounded by ADR 0004 #1 (the pull reads only namespaces newer than `pull_max_age_days`,
  at most `pull_max_namespaces`) and #4 (Coldline after 90 days, Delete after 365).
- **Rebuild continuity is limited by that pull filter.** A predecessor's history is restored only while
  its last push is within `pull_max_age_days` (default 30) and it ranks in the top `pull_max_namespaces`
  (default 15). A rebuild after a longer gap restores nothing from it unless `pull_max_age_days` is raised.
  The data remains in the bucket until the 365-day Delete rule.
- **Arrival is limited.** The pull runs at boot, and every `pull_interval_seconds` after that
  ([ADR 0008](./0008-periodic-pull.md)), so a running workstation sees a session another machine
  started within about that interval. An existing local transcript is never updated (`cp -an`), so a
  session continued elsewhere stays at the older local copy.
- **Duplicate storage** is unchanged: a workstation re-pushes the sessions it pulled, so an active session
  exists once per workstation that has seen it.
- **The ADR 0006 operator tooling and tests are gone with the revert** (`claude-sync pull|push|status`,
  `pull.json`, the baked `lib.sh`, `tests/run.sh`, `soft_delete_days`). None of them depends on this
  decision, and any can be reintroduced on its own.

## Implementation status

Reverting PR #34 alone returned the scripts to a state that still preferred
`RA_PLUGIN_CLAUDE_SYNC_NAMESPACE`, so the decision was not yet in force. It is now:

- `270_*.sh` and `271_*.sh` call `resolve_ws_id()` unconditionally and ignore
  `RA_PLUGIN_CLAUDE_SYNC_NAMESPACE`;
- `plugin.yaml` no longer declares `stable_id:`, so `ra create` stops injecting the id.

Rollout:

- **No config change is needed.** Workstation configs made by older `ra create` runs still carry
  `RA_PLUGIN_CLAUDE_SYNC_NAMESPACE` in their container env. The scripts ignore it.
- **First boot with this change:** each workstation starts pushing to a new namespace named after its
  instance, and its first push uploads the whole `~/.claude` once.
- **The old shared `<stable-id>` namespace** receives no more writes. It is pulled like any other
  namespace while it is within `pull_max_age_days` and the count cap, then ages out under ADR 0004 #4.
- **Nothing is recovered or migrated.** If two workstations were sharing a namespace, whatever each
  overwrote of the other's `history.jsonl` / `claude.json` stays lost.
