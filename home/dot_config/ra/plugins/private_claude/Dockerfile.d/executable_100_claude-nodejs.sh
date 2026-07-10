#!/bin/bash
# Claude plugin — install Node.js 20 at image build time.
# Required because Claude Code is distributed as an npm package.
set -euo pipefail

# Download to a file before running it: piping `curl --retry` straight into
# bash would re-emit from byte 0 on a mid-transfer retry, feeding bash a
# corrupted/duplicated script. With a file, the retry just re-downloads cleanly.
setup=$(mktemp)
trap 'rm -f "${setup}"' EXIT
# Retry transient network failures (e.g. 5xx) instead of aborting the build.
curl --retry 5 --retry-connrefused --connect-timeout 30 -fsSL https://deb.nodesource.com/setup_20.x -o "${setup}"
bash "${setup}"
apt-get install -y --no-install-recommends nodejs
rm -rf /var/lib/apt/lists/*
