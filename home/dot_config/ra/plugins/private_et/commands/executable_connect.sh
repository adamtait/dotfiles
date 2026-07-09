#!/usr/bin/env bash
# ra plugin et connect — connect to the workstation through the local Eternal
# Terminal (`et`) client instead of plain SSH, so a dropped connection
# auto-reconnects and resumes the session.
#
# `ra` invokes this after resolving and starting the workstation, with the RA_*
# env contract set (RA_WORKSTATION/PROJECT/REGION/CLUSTER/CONFIG/WORKSTATION_USER/
# LOGIN_COMMAND) and the `ra` binary on PATH. et needs two connections to the box
# — an ssh bootstrap to :22 and the etserver data channel to :2022 — and gcloud
# only tunnels one local→one remote port, so we bring up two local tunnels
# (supervised, so et always finds a live endpoint) and point `et` at them.
set -euo pipefail
set -m # job control: `kill %n` hits each background job's whole process group

command -v et >/dev/null 2>&1 || {
	echo "et: the local Eternal Terminal client is not installed." >&2
	echo "    Install it: brew install et   (https://eternalterminal.dev/)" >&2
	exit 1
}
: "${RA_WORKSTATION:?ra did not set RA_WORKSTATION}"

readonly REMOTE_SSH_PORT=22
readonly REMOTE_DATA_PORT=2022 # etserver; matches the plugin's port-for-tunnel
readonly USER_NAME="${RA_WORKSTATION_USER:-user}"
loc=(--project "$RA_PROJECT" --cluster "$RA_CLUSTER" --config "$RA_CONFIG")

# free_port prints an unused local TCP port (a connect that fails ⇒ nothing is
# listening ⇒ free). Racy in principle, same as any pick-then-bind.
free_port() {
	local p
	for _ in $(seq 1 100); do
		p=$(((RANDOM % 16384) + 49152))
		if ! (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null; then
			echo "$p"
			return 0
		fi
		exec 3>&- 2>/dev/null || true
	done
	echo "et: could not find a free local port" >&2
	return 1
}
BOOT_PORT="$(free_port)"
DATA_PORT="$(free_port)"
while [ "$DATA_PORT" = "$BOOT_PORT" ]; do DATA_PORT="$(free_port)"; done

# Supervised tunnel: `ra tunnel` is a single foreground (unsupervised) tunnel, so
# wrap it in a respawn loop — if it drops, et's own reconnect finds a fresh one.
supervise() {
	local lport="$1" rport="$2" label="$3" delay=1 start
	while true; do
		start=$SECONDS
		ra tunnel "${lport}:${rport}" "$RA_WORKSTATION" "${loc[@]}" >/dev/null 2>&1 || true
		if [ $((SECONDS - start)) -ge 2 ]; then
			delay=1 # ran a while ⇒ a real drop; reconnect promptly
		else
			delay=$((delay < 30 ? delay * 2 : 30)) # fast-fail (e.g. `ra` missing) ⇒ capped backoff
		fi
		echo "et: ${label} tunnel dropped; reconnecting in ${delay}s..." >&2
		sleep "$delay"
	done
}
supervise "$BOOT_PORT" "$REMOTE_SSH_PORT" bootstrap &
supervise "$DATA_PORT" "$REMOTE_DATA_PORT" data &
trap 'kill %1 %2 2>/dev/null || true; wait 2>/dev/null || true' EXIT INT TERM

# Wait for a local tunnel port to start accepting connections.
wait_port() {
	local port="$1" deadline=$((SECONDS + 30))
	while [ "$SECONDS" -lt "$deadline" ]; do
		if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
			exec 3>&- 2>/dev/null || true
			return 0
		fi
		sleep 0.25
	done
	return 1
}
wait_port "$BOOT_PORT" || echo "et: bootstrap tunnel not up yet; continuing..." >&2
wait_port "$DATA_PORT" || echo "et: data tunnel not up yet; continuing..." >&2

# etserver comes up after sshd, so gate on it (no ss/netstat in the image, so
# read /proc/net/tcp[6] for a LISTEN socket — state 0A — on 2022 == hex 07E6).
# et would retry anyway; this just avoids a confusing initial failure.
# shellcheck disable=SC2016  # the awk program must reach the remote unexpanded
probe='awk -v p=":07E6" '\''$2 ~ (p"$") && $4 == "0A" {f=1} END {if (f) print "ra_et_ready"}'\'' /proc/net/tcp /proc/net/tcp6 2>/dev/null'
et_deadline=$((SECONDS + 120))
until gcloud workstations ssh "$RA_WORKSTATION" "${loc[@]}" --region "$RA_REGION" --command "$probe" 2>/dev/null | grep -q ra_et_ready; do
	if [ "$SECONDS" -ge "$et_deadline" ]; then
		echo "et: etserver not confirmed on port ${REMOTE_DATA_PORT}; connecting anyway (et will retry)..." >&2
		break
	fi
	sleep 3
done

# Build the et argv (bootstrap port as an ssh -o Port=, data channel via -p).
args=(-k 3 --ssh-option "Port=${BOOT_PORT}")
ident="${HOME}/.ssh/google_compute_engine"
[ -r "$ident" ] && args+=(--ssh-option "IdentityFile=${ident}")
args+=(--ssh-option StrictHostKeyChecking=no --ssh-option UserKnownHostsFile=/dev/null -p "${DATA_PORT}")
# No --command for et. Unlike core `ra connect` (whose `ssh --command` runs a
# NON-login shell, so RA_LOGIN_COMMAND is how tmux gets launched), et connects
# through a real *login* shell: the workstation's normal startup runs
# (/etc/profile.d/*, incl. the tmux plugin's auto-attach), so tmux — or whatever
# the login shell does — is already up. Passing RA_LOGIN_COMMAND here is both
# redundant and breaking: et's -c/--command is run-and-exit (not a PTY login
# shell like `ssh -t`), so it drops the session immediately, and re-launching
# tmux inside the already-attached session errors ("sessions should be nested
# with care"). RA_LOGIN_COMMAND is intentionally unused for the et transport.

echo "Connecting to ${RA_WORKSTATION} via Eternal Terminal (auto-reconnecting)..." >&2
# Foreground (not exec) so the EXIT trap tears the tunnels down when et returns.
et "${args[@]}" "${USER_NAME}@localhost"
