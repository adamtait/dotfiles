# 8. Periodic pull

- Status: accepted (2026-11-06)
- Builds on: [ADR 0002](./0002-merge-based-sync.md), [ADR 0004](./0004-load-reduction.md) #1 and
  [ADR 0007](./0007-unique-workstation-identifiers.md)
- Applies to: `claude-sync` plugin

## Context

ADR 0007 makes the cross-namespace pull the only thing that carries history between workstations. That
pull ran once, at boot. A workstation that stays up for days therefore never saw a session started on
another machine until its next reboot.

## Decision

1. **The push loop starts the pull every `pull_interval_seconds`** (new config key, default `600`;
   `0` = boot pull only). It runs `270_*.sh --periodic` in the background, so a slow pull never delays
   a push and a failed pull costs one round. The first one is a full interval after the loop starts.
2. **Same script, same merge, same filter as the boot pull.** Own namespace skipped, age and count
   filter (ADR 0004 #1), `cp -an` so local files win, `history.jsonl` union-merged. Two differences:
   it does not detach or tee (its stdout already is the loop's log), and it **does not touch
   `.claude.json`**: Claude Code rewrites that file constantly and the local copy is authoritative, so
   other machines' per-project history still arrives only at boot.
3. **Incremental downloads.** The directory stage lives in `/var/cache/ra-claude-sync/stage` and
   persists between pulls, so `gcloud storage rsync` fetches only what changed. The single-file stage
   (history copies) is rebuilt each run, so an aged-out namespace stops feeding the merge.
4. **Unchanged namespaces are skipped.** The pull records each namespace's manifest epoch after a
   successful `projects/` sync and skips a namespace whose epoch hasn't moved. The manifest only
   advances after a successful push, so nothing new can be behind it. A live workstation advances it
   every push cycle and is always re-pulled; an idle or stopped one costs no `gcloud` calls. The boot
   pull never skips, and a namespace without a manifest or whose `projects/` sync failed is never
   recorded, so it is retried, not missed.
5. **One pull at a time**, via `flock` on `/run/ra-claude-sync.pull.lock`. A pull that finds the lock
   held (the boot pull, or a slow previous periodic pull) logs it and skips the round.
6. **`history.jsonl` write-back is guarded** (boot pull too), because Claude Code appends to it while a
   periodic pull runs: nothing is written when the merge changes nothing; the replacement keeps the
   file's owner and mode, so it never becomes root-owned; and if the file's size, inode or mtime changed
   during the merge the result is discarded and the merge retried (3 attempts).

## Consequences

- **New sessions arrive, existing transcripts don't update.** `cp -an` never overwrites, so a session you
  continue on another machine stays at the older copy here. Propagating that would mean replacing a file
  Claude Code may be appending to, which needs its own checks and its own decision. (PR #35's
  fast-forward merge was one such design; it was closed unmerged.)
- **A transcript you delete locally can reappear** within one interval (previously at the next boot),
  because `cp -an` restores anything missing from the stage.
- **Cost scales with live peers.** Each interval costs one `ls` and one manifest `rsync`, plus about five
  `gcloud` calls for each *changed* namespace. At the defaults the worst case, every one of 15 namespaces
  live, is about 75 calls per interval.
- **Disk:** the stage holds the union of the selected namespaces' files, bounded by
  `pull_max_namespaces` (about 85 MB per namespace, ADR 0004), and resets with the container.
- **A file that differs between namespaces is re-downloaded every pull** while its peers are live, because
  all namespaces are synced into one stage tree, as at boot.
- **Copies into `~/.claude` are not atomic.** A push cycle landing mid-copy can upload a truncated file,
  which the next cycle corrects; a peer that pulled it in that window keeps the truncated copy.
- **A race of microseconds** remains between the final unchanged check and the rename of `history.jsonl`.
- **Incrementality is not verified against real GCS.** It relies on `gcloud storage rsync` skipping
  files it already has. It was tested only against a fake `gcloud` that compares content.
