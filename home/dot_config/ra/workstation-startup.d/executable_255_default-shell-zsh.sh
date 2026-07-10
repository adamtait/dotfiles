#!/bin/bash
# Default login shell -> zsh, reasserted on every boot.
#
# Why a boot-time hook and not the image Dockerfile:
#   - The `user` account is created at container *startup* by the Cloud
#     Workstations entrypoint, not baked into the image. A Dockerfile
#     `chsh user` therefore fails at build time ("user 'user' does not exist").
#   - /etc/passwd lives on the ephemeral overlay rootfs, so the login shell
#     resets to the image default (/bin/bash) on every rebuild.
# Setting it here — as root, after the user exists, on every boot — is the only
# durable fix. zsh itself is installed in the image (see the Dockerfile apt
# layer), so it is always present by the time this runs.
#
# Idempotent and non-fatal: a no-op once the shell is already zsh, and a warning
# (never a hard failure) if usermod is blocked, so it can never wedge boot.
set -uo pipefail

ZSH_BIN=/usr/bin/zsh
USER_NAME=user

log() { echo "[default-shell] $*"; }

if ! command -v zsh >/dev/null 2>&1; then
    log "WARNING: zsh not found on PATH; leaving default shell unchanged" >&2
    exit 0
fi

# Register zsh in /etc/shells (also ephemeral) so anything that validates a
# login shell against it — chsh, some PAM setups — accepts it.
if ! grep -qxF "${ZSH_BIN}" /etc/shells 2>/dev/null; then
    echo "${ZSH_BIN}" >> /etc/shells || log "WARNING: could not append ${ZSH_BIN} to /etc/shells" >&2
fi

current_shell="$(getent passwd "${USER_NAME}" 2>/dev/null | cut -d: -f7)"
if [ "${current_shell}" = "${ZSH_BIN}" ]; then
    exit 0
fi

if usermod -s "${ZSH_BIN}" "${USER_NAME}"; then
    log "set login shell for ${USER_NAME} to ${ZSH_BIN}"
else
    log "WARNING: failed to set login shell for ${USER_NAME}; leaving as ${current_shell:-unknown}" >&2
fi
