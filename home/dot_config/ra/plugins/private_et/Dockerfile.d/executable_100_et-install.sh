#!/bin/bash
# et plugin — install the Eternal Terminal server at image build time.
#
# Eternal Terminal (https://eternalterminal.dev/) is a remote shell that
# automatically reconnects across network drops, sleeps, and IP roaming.
# `ra connect --et` runs the local `et` client against this server — through a
# pair of gcloud TCP tunnels — so a dropped workstation connection resumes the
# session instead of freezing.
#
# Installs both `etserver` (the daemon, started at boot by 250_et-start.sh) and
# `etterminal` (the per-session helper the `et` client invokes over ssh during
# the connection handshake) from the upstream PPA.
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export ET_NO_TELEMETRY=1

apt-get update
apt-get install -y --no-install-recommends software-properties-common
add-apt-repository -y ppa:jgmath2000/et
apt-get update
# `et` is the server/client; `iproute2` provides `ss`, which the base ra image
# does not ship — having it makes the workstation's network state inspectable
# (e.g. `ss -tlnp`) when debugging a tunnel. The boot script's port check does
# not depend on it (it reads /proc/net/tcp directly); this is for operability.
apt-get install -y --no-install-recommends et iproute2

# The package ships a systemd unit, but ra workstations do not run systemd
# (PID 1 is the workstation entrypoint), so that unit never starts. Boot-time
# startup is handled by 250_et-start.sh instead; drop the unit's enablement
# symlink so it can't cause confusion later.
rm -f /etc/systemd/system/multi-user.target.wants/et.service 2>/dev/null || true

etserver --version
