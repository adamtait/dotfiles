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

# LOG_FILE must be set before the detach guard — it redirects the detached
# child's output at re-exec time. All other variables live below the guard.
LOG_FILE="/var/log/ra-dotfiles-install.log"

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

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"
INSTALL_MARKER="${USER_HOME}/.ra-dotfiles-installed"
CHEZMOI_BIN="${USER_HOME}/.local/bin/chezmoi"
CHEZMOI_SOURCE_DIR="${USER_HOME}/.local/share/chezmoi"
CHEZMOI_CONFIG_DIR="${USER_HOME}/.config/chezmoi"
CHEZMOI_CONFIG_FILE="${CHEZMOI_CONFIG_DIR}/chezmoi.toml"

log() { echo "[dotfiles-install] $(date -u +%H:%M:%S) $*"; }

# Source fetched secrets and propagated env vars (RA_PLUGIN_* toggles, etc.)
# so that a future enable/disable gate or any hook that needs a secret can
# see them — same preamble used by all sibling workstation-startup.d scripts.
[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

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
chmod 0600 "${CHEZMOI_CONFIG_FILE}"
log "pre-seeded ${CHEZMOI_CONFIG_FILE}"

# If the source dir already exists as a valid git repo (e.g. from a prior boot
# where clone succeeded but apply failed mid-run), pull updates rather than
# wiping it — this preserves any local edits made between attempts.
# Only rm -rf if the dir exists but is not a valid git repo (corrupt partial clone).
if [ -d "${CHEZMOI_SOURCE_DIR}" ] && \
        sudo -u "${USER_NAME}" git -C "${CHEZMOI_SOURCE_DIR}" rev-parse --git-dir >/dev/null 2>&1; then
    log "source dir exists; pulling latest"
    if ! sudo -u "${USER_NAME}" env HOME="${USER_HOME}" \
            git -C "${CHEZMOI_SOURCE_DIR}" pull; then
        log "ERROR: git pull failed; will retry on next boot"
        exit 1
    fi
    log "running chezmoi apply"
    if ! sudo -u "${USER_NAME}" env HOME="${USER_HOME}" \
            "${CHEZMOI_BIN}" apply --no-tty; then
        log "ERROR: chezmoi apply failed; will retry on next boot"
        exit 1
    fi
else
    [ -d "${CHEZMOI_SOURCE_DIR}" ] && { log "removing invalid source dir"; rm -rf "${CHEZMOI_SOURCE_DIR}"; }
    log "running chezmoi init --apply"
    if ! sudo -u "${USER_NAME}" env HOME="${USER_HOME}" \
            "${CHEZMOI_BIN}" init --apply --no-tty \
            https://github.com/adamtait/dotfiles.git; then
        log "ERROR: chezmoi init --apply failed; will retry on next boot"
        exit 1
    fi
fi

touch "${INSTALL_MARKER}"
chown "${USER_NAME}:${USER_NAME}" "${INSTALL_MARKER}"
log "done"
