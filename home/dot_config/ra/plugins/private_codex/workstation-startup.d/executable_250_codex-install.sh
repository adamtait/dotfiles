#!/bin/bash
# Codex plugin — first-boot install via the standalone installer.
#
# Runs as part of /etc/workstation-startup.d/, after secrets are written
# by 200_remote-agent-setup.sh. Bails fast on subsequent boots once the
# codex binary is present.
#
# The installer (curl -fsSL https://chatgpt.com/codex/install.sh | sh) drops a
# self-contained binary — no Node.js/npm needed. It installs unprivileged into
# $HOME: the codex entry point lands at ~/.local/bin/codex (a symlink into
# ~/.codex/packages/standalone/current), both under the persistent /home disk.
# 100_codex-path-link.sh baked the /usr/local/bin/codex symlink pointing there.
set -euo pipefail

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"
CODEX_BIN="${USER_HOME}/.local/bin/codex"

# Source fetched secrets in case downstream scripts care; not strictly
# needed for the install itself.
[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_CODEX_ENABLED:-false}" = "true" ] || exit 0

# Fast path / idempotency, keyed on the binary itself (not a marker file). This
# also makes a workstation upgrading off the old npm-based plugin self-heal: its
# codex lives at the old ~/.npm-global path, so CODEX_BIN is absent and the
# standalone installer runs, populating ~/.local/bin and resolving the rebaked
# /usr/local/bin/codex symlink. Every later boot then bails here.
[ -x "${CODEX_BIN}" ] && exit 0

# Download the installer to a file before running it: piping `curl --retry`
# straight into sh would re-emit from byte 0 on a mid-transfer retry, feeding sh
# a corrupted/duplicated script. With a file, the retry just re-downloads cleanly.
setup=$(mktemp)
trap 'rm -f "${setup}"' EXIT
# Retry transient network failures (e.g. 5xx) instead of aborting the boot. -L
# follows the chatgpt.com -> github.com/openai/codex releases redirect.
curl --retry 5 --retry-connrefused --connect-timeout 30 \
    -fsSL https://chatgpt.com/codex/install.sh -o "${setup}"
chmod 0644 "${setup}"

# Run the installer as the user so it writes into their persistent $HOME.
# CODEX_NON_INTERACTIVE declines the installer's "Start Codex now?" / uninstall
# prompts, which would otherwise hang a headless boot waiting on /dev/tty.
# CODEX_RELEASE pins the version (RA_PLUGIN_CODEX_RELEASE config); 'latest' —
# the config default, and the fallback if unset — installs the newest release.
# PATH carries ~/.local/bin so the installer sees its target dir already on PATH
# and skips appending an `export PATH=...` block to a shell dotfile: this plugin
# puts codex on PATH via the /usr/local/bin symlink, never shell config (ADR 0002).
sudo -u "${USER_NAME}" \
    env HOME="${USER_HOME}" CODEX_NON_INTERACTIVE=1 \
    CODEX_RELEASE="${RA_PLUGIN_CODEX_RELEASE:-latest}" \
    PATH="${USER_HOME}/.local/bin:${PATH}" \
    sh "${setup}"
