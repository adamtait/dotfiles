#!/bin/bash
# et plugin — start the Eternal Terminal server (etserver) at container boot.
#
# Runs as root from /etc/workstation-startup.d/, after 200_remote-agent-setup.sh
# has sourced config into /run/ra/env. etserver runs as root: it owns the
# listening port and spawns each session's shell as the user authenticated by
# the ET handshake, which rides on the workstation's existing sshd (the same
# sshd `gcloud workstations ssh` uses). It is detached with setsid so it
# survives this script's exit.
#
# Idempotency: pgrep on etserver — running the script twice on the same boot is
# a no-op on the second run.
#
# Logging: appends to /var/log/et-start.log.
set -euo pipefail

ET_LOG="/var/log/et-start.log"

[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_ET_ENABLED:-false}" = "true" ] || exit 0

port="${RA_PLUGIN_ET_PORT_FOR_TUNNEL:-2022}"

# port_listening <port> — succeeds if a local socket is in LISTEN state on the
# port. Reads the kernel's own table (/proc/net/tcp + tcp6) directly: the ra
# workstation image ships neither `ss` nor `netstat`, so parse what they would
# parse. Local addresses there are HEXIP:HEXPORT and LISTEN is state 0A, so the
# port is matched as an uppercase 4-digit hex suffix. Passive — no connection is
# opened against whatever holds the port.
port_listening() {
    local hex
    hex=$(printf ':%04X' "$1")
    awk -v p="${hex}" '$2 ~ (p"$") && $4 == "0A" { found = 1 } END { exit(found ? 0 : 1) }' \
        /proc/net/tcp /proc/net/tcp6 2>/dev/null
}

# Validate the port. The schema declares the tunnel port, but RA_PLUGIN_* values
# are unquoted env strings and nothing re-checks them on the workstation side.
if [[ ! "${port}" =~ ^[0-9]+$ ]] || [ "${port}" -lt 1 ] || [ "${port}" -gt 65535 ]; then
    echo "[et-start] invalid port '${port}'; must be 1-65535" >&2
    exit 1
fi

# Idempotency: a previous boot (or a previous run of this script) may have left
# etserver running.
if pgrep -x etserver >/dev/null 2>&1; then
    echo "[et-start] etserver already running; skipping."
    exit 0
fi

# Port collision pre-check — without this, etserver bind-fails into the log and
# the user sees an unreachable port with no obvious cause.
if port_listening "${port}"; then
    echo "[et-start] port ${port} already in use; aborting." >&2
    exit 1
fi

# Launch detached:
#   - ET_NO_TELEMETRY disables upstream crash/usage telemetry.
#   - setsid --fork puts etserver in its own session/process-group so it
#     survives this script's exit and any SIGHUP from session teardown.
#   - exec etserver replaces bash so there's no extra shell PID under pgrep.
#   - </dev/null detaches stdin so the daemon has no controlling tty.
#   - --port is required: etserver defaults to port 0 unless told otherwise.
env ET_NO_TELEMETRY=1 \
    setsid --fork bash -c \
    "exec etserver --port ${port} >> '${ET_LOG}' 2>&1" \
    </dev/null

echo "[et-start] started etserver on port ${port}"
