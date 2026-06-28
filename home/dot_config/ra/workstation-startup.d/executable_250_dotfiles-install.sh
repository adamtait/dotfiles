#!/bin/bash
# Dotfiles — first-boot install via chezmoi.
#
# Runs as part of /etc/workstation-startup.d/ (as root) after secrets are
# written by 200_remote-agent-setup.sh. Bails fast on subsequent boots via
# the install marker (~/.ra-dotfiles-installed). The home disk is persistent
# across workstation recreations, so the marker survives container rebuilds.
#
# The actual install runs detached in the background (setsid --fork pattern,
# same as 270_claude-sync-pull.sh) because chezmoi --apply runs post-install
# hooks for pyenv, bun, etc. that can take several minutes and would otherwise
# block the workstation-startup.d sequence long enough for sshd to never
# stabilize.
set -euo pipefail

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"
INSTALL_MARKER="${USER_HOME}/.ra-dotfiles-installed"
LOG_FILE="/var/log/ra-dotfiles-install.log"
CHEZMOI_BIN="${USER_HOME}/.local/bin/chezmoi"
CHEZMOI_SOURCE_DIR="${USER_HOME}/.local/share/chezmoi"
CHEZMOI_CONFIG_DIR="${USER_HOME}/.config/chezmoi"
CHEZMOI_CONFIG_FILE="${CHEZMOI_CONFIG_DIR}/chezmoi.toml"

# Detach so the install never blocks workstation startup.
# chezmoi --apply runs post-install hooks (pyenv, bun, package installs)
# that take several minutes. Running inline blocks the workstation-startup.d
# sequence long enough that Cloud Workstations tears the container down
# mid-boot — sshd never stabilizes and `ra connect` fails. Re-exec once,
# detached into a new session via `setsid --fork`, and return immediately so
# boot completes. RA_DOTFILES_INSTALL_DETACHED guards against infinite re-exec.
if [ "${RA_DOTFILES_INSTALL_DETACHED:-0}" != "1" ]; then
    export RA_DOTFILES_INSTALL_DETACHED=1
    if ! setsid --fork "$0" </dev/null >>"${LOG_FILE}" 2>&1; then
        echo "[dotfiles-install] WARNING: detached re-exec of '$0' failed;" \
             "dotfiles install skipped this boot" >&2
    fi
    exit 0
fi

exec >>"${LOG_FILE}" 2>&1

log() { echo "[dotfiles-install] $(date -u +%H:%M:%S) $*"; }

log "starting"

[ -e "${INSTALL_MARKER}" ] && { log "already installed; exiting"; exit 0; }

# Install chezmoi binary to ~/.local/bin if not already on the persistent disk.
if [ ! -x "${CHEZMOI_BIN}" ]; then
    log "installing chezmoi to ${CHEZMOI_BIN}"
    sudo -u "${USER_NAME}" env HOME="${USER_HOME}" \
        sh -c "$(curl -fsSL https://get.chezmoi.io)" -- -b "${USER_HOME}/.local/bin"
    log "chezmoi installed"
fi

# Pre-seed chezmoi config to bypass interactive prompts.
# chezmoi.toml.tmpl uses promptStringOnce / promptChoiceOnce for name, email,
# and machine. When those keys are already present in the config file,
# chezmoi skips the prompt entirely — the same technique used in CI
# (.github/workflows/ci.yml "Seed chezmoi config" step).
sudo -u "${USER_NAME}" mkdir -p "${CHEZMOI_CONFIG_DIR}"
sudo -u "${USER_NAME}" tee "${CHEZMOI_CONFIG_FILE}" >/dev/null <<'TOML'
[data]
    name = "adamtait"
    email = "bin@adamta.it"
    machine = "cloud-workstation"
    isCloud = true
TOML
chown "${USER_NAME}:${USER_NAME}" "${CHEZMOI_CONFIG_FILE}"
chmod 0600 "${CHEZMOI_CONFIG_FILE}"
log "pre-seeded ${CHEZMOI_CONFIG_FILE}"

# Remove any stale source dir from a prior failed attempt. chezmoi init
# refuses to overwrite an existing source dir, so a failed clone would
# block all future retries without this cleanup.
if [ -d "${CHEZMOI_SOURCE_DIR}" ]; then
    log "removing stale source dir from previous attempt"
    rm -rf "${CHEZMOI_SOURCE_DIR}"
fi

log "running chezmoi init --apply"
if sudo -u "${USER_NAME}" env HOME="${USER_HOME}" \
        "${CHEZMOI_BIN}" init --apply --no-tty \
        https://github.com/adamtait/dotfiles.git; then
    log "chezmoi init --apply succeeded"
else
    log "ERROR: chezmoi init --apply failed; will retry on next boot"
    exit 1
fi

touch "${INSTALL_MARKER}"
chown "${USER_NAME}:${USER_NAME}" "${INSTALL_MARKER}"
log "done"
