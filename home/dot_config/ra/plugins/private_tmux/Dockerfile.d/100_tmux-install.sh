#!/bin/bash
# tmux plugin — install tmux at image build time.
set -euo pipefail

apt-get update
# ncurses-term ships terminfo entries for modern terminals (xterm-ghostty,
# xterm-kitty, wezterm, alacritty, …). With it installed tmux starts cleanly
# under those TERM values without any fallback. ncurses-bin still provides
# infocmp, which 05-tmux-autoattach.sh uses as a last-resort detector for
# any TERM that ncurses-term does not yet cover, falling back to
# xterm-256color before launching tmux. tmux itself only depends on
# libtinfo6, so we ask for both packages explicitly to keep behavior robust
# under base-image churn.
apt-get install -y --no-install-recommends tmux ncurses-bin ncurses-term
rm -rf /var/lib/apt/lists/*

# Force every pane to spawn a *login* shell. Without this, tmux spawns
# interactive non-login shells, which do not source /etc/profile.d/* — so
# /run/ra/env (sourced by 00-ra-wait.sh) and any plugin secrets exported
# there would be missing in newly-opened windows/splits even though the
# initial attached shell has them.
#
# Resolve the shell dynamically from /etc/passwd at pane-open time rather than
# hardcoding a path. The tmux server is persistent (started once per boot by
# 250_tmux-start.sh) and the workstation's login shell can change after this
# image is built — e.g. workstation-startup.d/255_default-shell-zsh.sh usermods
# `user` to zsh *after* tmux has already started. A hardcoded "/bin/bash -l"
# would pin bash for the server's whole life, so every pane on every attach
# (`ra connect`, et) would run bash regardless of the real login shell. Reading
# it live keeps the plugin shell-agnostic and correct across that change.
cat > /etc/tmux.conf <<'EOF'
# ra tmux plugin: ensure new panes are login shells so /etc/profile.d/* runs
# (which sources /run/ra/env), keeping RA_PLUGIN_* and plugin secrets
# available in every window/split, not just the one that exec'd tmux.
# The shell is resolved from /etc/passwd at pane-open time (not hardcoded) so it
# tracks any post-build chsh/usermod change; `exec` leaves no extra sh wrapper.
# Fall back to bash if the lookup ever comes back empty — an empty value would
# make `exec` fail and the pane die with no shell, whereas bash always exists.
set -g default-command '_sh=$(getent passwd "$(id -un)" | cut -d: -f7); exec "${_sh:-/bin/bash}" -l'

# Keep the tmux server alive across transient zero-session states. tmux defaults
# to `exit-empty on`, which reaps the whole server the moment its last session
# closes — e.g. if the boot session (250_tmux-start.sh) has its pane shell exit —
# leaving a stale socket and a confusing "no server running" state with no warm
# session. Setting it here (rather than in the boot script) applies to every
# server at startup, idempotently, and cannot fail the startup script. See #14.
set -g exit-empty off
EOF
chmod 0644 /etc/tmux.conf
