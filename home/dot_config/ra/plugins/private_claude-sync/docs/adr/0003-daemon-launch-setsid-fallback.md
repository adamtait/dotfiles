# 3. Daemon launch: setsid fallback when systemd is not PID 1

- Status: accepted (2026-05-28)
- Applies to: `claude-sync` plugin, `workstation-startup.d/271_claude-sync-daemon.sh`

## Context

On every workstation provisioned since the plugin shipped, the continuous push loop **never started**,
even though the boot-time pull ran correctly and both the systemd unit and the loop binary were
materialized on disk. The bucket's `workstations/` prefix was empty fleet-wide — no workstation had
ever pushed.

Root cause: the Cloud Workstations default base image runs its own entrypoint as PID 1, **not
systemd**. `systemctl is-active` reports *"System has not been booted with systemd as init system
(PID 1)"*. `271_*.sh` called `systemctl daemon-reload && systemctl enable --now && systemctl restart`,
all of which failed silently under the trailing `|| true`, so the unit file existed but nothing ever
launched it. The pull side worked because it executes directly during boot, not via systemd. This
exact failure mode had been anticipated ("if systemd is not available, fall back to a background loop")
but the fallback was never implemented.

## Decision

Detect PID 1 in `271_*.sh` and choose the launch strategy at runtime:

- `[ "$(cat /proc/1/comm)" = "systemd" ]` → the existing systemd path (preserves correct behavior on
  any future image that does boot systemd as PID 1).
- otherwise → `setsid --fork "${LOOP_PATH}" </dev/null`, mirroring the marimo plugin's daemon-launch
  pattern.

**Idempotency uses a pidfile, not `pgrep -f`.** The loop writes its own PID to
`/run/ra-claude-sync.pid` as one of its first actions and clears it via an `EXIT` trap. `271_*.sh`
reads the file and `kill`s the previous loop (5s SIGTERM grace, then SIGKILL) with a `kill -0` liveness
check first. `/run` is cleared on reboot, so stale pidfiles auto-clean. The first draft used
`pgrep -f "ra-claude-sync-loop\.sh"` / `pkill -f`, but that regex also matches the **calling shell's
own command line** whenever its argv mentions the loop path (e.g. a developer who just `cat`ed the
script), so a `pkill -f` from inside `271` could SIGTERM the calling shell. The pidfile — a single
integer read from a root-only-writable file — has no pattern-matching risk.

## Consequences

- **No supervisor in the setsid path.** Under systemd, `Restart=always` would respawn the loop after any
  death; under setsid, if the loop exits it stays dead until the next reboot re-runs `271`. Accepted:
  the loop's early-exit branches are preconditions that `271` itself re-validates before launching, its
  body wraps every gcloud call in `|| true` so transient failures don't kill it, and a real death
  (signal/OOM) on a single-tenant workstation is rare with well-established reboot recovery. If
  production loop deaths are observed, the next step is a small respawn wrapper in `271`, not a change
  to the loop body.
- **Two-path complexity.** The script branches on systemd availability. The systemd path is currently
  dead code on this image but is kept because it costs nothing and preserves correctness if Cloud
  Workstations switches to systemd-as-PID-1 (the documented default for several newer image variants).
- **Residual pidfile corners**, all accepted as operator-visible (`pgrep -af ra-claude-sync-loop`) and
  near-impossible to hit non-deliberately: a one-time double-loop if `271` is manually re-run against a
  pre-fix loop that wrote no pidfile; a launch race if `271` is invoked twice within milliseconds before
  the first loop writes its pidfile; and stale-pidfile PID reuse if the loop is SIGKILL'd (skipping the
  trap) and the kernel reuses the PID before the next reboot clears `/run`. Closing these would require
  flock/comm cross-checks that reintroduce the pattern-matching risks this design deliberately avoids.

## Verification (live)

Reproduced the empty-bucket bug on a workstation with PID 1 = the base-image entrypoint. With the fix:
the setsid branch fires, the loop starts and writes its pidfile, and within seconds the workstation's
namespace is populated in GCS for the first time; re-running `271` cleanly stops the old loop and
starts a new one, leaving exactly one loop process and a fresh manifest timestamp.
