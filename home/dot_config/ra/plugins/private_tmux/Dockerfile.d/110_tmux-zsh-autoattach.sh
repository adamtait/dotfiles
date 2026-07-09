#!/bin/bash
# tmux plugin — install a zsh login-shell auto-attach hook.
#
# Why this exists (issue #13):
#   The plugin's bash auto-attach ships as /etc/profile.d/05-tmux-autoattach.sh,
#   which is sourced only by *bash* login shells (via /etc/profile). When the
#   workstation's login shell is zsh — e.g. a dotfiles step runs `chsh` — that
#   hook never fires and tmux does not start on `ra connect` / `--et`. zsh login
#   shells source /etc/zsh/{zshenv,zprofile,zshrc,zlogin} instead, never
#   /etc/profile.d/*, so the plugin needs a native-zsh analog.
#
# Why a dpkg Post-Invoke hook rather than a startup script:
#   On ra workstations zsh is typically installed at *boot*, by an external
#   dotfiles step that detaches itself (setsid --fork) — so it finishes at an
#   unpredictable time, after the (sequential, must-not-block) startup scripts
#   have run. And /etc/zsh/zlogin is a `zsh-common` dpkg conffile, so we cannot
#   pre-create it at build time (a later `apt-get install zsh` would clobber it)
#   nor assume it exists in a startup script. A dpkg Post-Invoke hook sidesteps
#   both problems: it runs right after *any* apt transaction — including the one
#   that installs zsh — with no dependence on startup ordering or timing. The
#   installer is idempotent and a no-op until zsh is present, so firing on every
#   apt run is harmless.
#
#   Assumption: zsh reaches the workstation via apt/apt-get (the ra dotfiles path
#   does). A bare `dpkg -i`, a snap, or a pre-baked/from-source zsh would not fire
#   Post-Invoke; the once-at-build run (step 4) covers zsh already present in the
#   image, but a runtime non-apt install would be missed — the per-user
#   ~/.zprofile hook remains the backstop there.
set -euo pipefail

SNIPPET=/etc/ra/tmux/zsh-autoattach.zsh
INSTALLER=/usr/local/sbin/ra-tmux-zsh-autoattach

# 1. The native-zsh snippet, sourced from /etc/zsh/zlogin once installed. Written
#    with a quoted heredoc so its shell syntax is stored verbatim (no expansion
#    at build time). This mirrors the bash 05-tmux-autoattach.sh logic.
mkdir -p "$(dirname "${SNIPPET}")"
cat > "${SNIPPET}" <<'RA_SNIPPET_EOF'
# ra tmux plugin — zsh login auto-attach (sourced from /etc/zsh/zlogin).
#
# The plugin's bash /etc/profile.d/05-tmux-autoattach.sh is not sourced by zsh
# login shells; this is the zsh analog. Gated on RA_PLUGIN_TMUX_ENABLED (exported
# into the environment via /etc/environment) so it is inert unless the tmux
# plugin is enabled. Guards: interactive tty (skip scp/non-interactive), not
# already inside tmux. On a clean detach it exits the login shell to close the
# connection, matching the bash hook.
if [[ "${RA_PLUGIN_TMUX_ENABLED:-}" == "true" && -t 1 && -z "${TMUX:-}" ]] && command -v tmux >/dev/null 2>&1; then
  # Known-good TERM fallback when the client's terminfo isn't installed
  # (Ghostty/Kitty/WezTerm), else tmux refuses to start and would drop the shell.
  infocmp -- "$TERM" &>/dev/null || export TERM=xterm-256color
  # Resolve the start dir; fall back to $HOME if it doesn't exist yet (as the
  # plugin's own scripts do) — e.g. a repo path set before it has been cloned.
  ra_tmux_dir="${${RA_PLUGIN_TMUX_START_DIRECTORY:-$HOME}/#\~/$HOME}"
  [[ -d "$ra_tmux_dir" ]] || ra_tmux_dir="$HOME"
  if tmux new-session -A -s "${RA_PLUGIN_TMUX_SESSION_NAME:-main}" -c "$ra_tmux_dir"; then
    unset ra_tmux_dir
    exit 0
  fi
  unset ra_tmux_dir
  # tmux failed to start -> fall through to a normal zsh login.
fi
RA_SNIPPET_EOF
chmod 0644 "${SNIPPET}"

# 2. Idempotent installer: append a guarded `source` line to /etc/zsh/zlogin.
#    No-op unless zsh is installed and the hook is not already present. Quoted
#    heredoc -> stored verbatim.
cat > "${INSTALLER}" <<'RA_INSTALLER_EOF'
#!/bin/bash
# Idempotently wire the ra tmux zsh auto-attach snippet into /etc/zsh/zlogin.
# Invoked from a dpkg Post-Invoke hook and once at image build. No-op unless zsh
# is installed and the hook is not already present.
#
# We append to /etc/zsh/zlogin, a zsh-common dpkg conffile (zsh has no global
# zlogin.d drop-in). The only side effect is that an in-place `apt upgrade` of
# zsh-common may show a conffile prompt (noninteractive keeps our version) —
# acceptable given the fresh-root-each-boot model. The guard greps for the
# load-bearing `source` line (not the header comment), and the block is written
# as a single printf, so a half-written block is repaired on the next run rather
# than matching the marker forever.
set -eu
ZLOGIN=/etc/zsh/zlogin
command -v zsh >/dev/null 2>&1 || exit 0
[ -d /etc/zsh ] || exit 0
grep -qsF 'source /etc/ra/tmux/zsh-autoattach.zsh' "${ZLOGIN}" 2>/dev/null && exit 0
printf '\n%s\n%s\n%s\n' \
  '# >>> ra tmux plugin: zsh auto-attach (managed) >>>' \
  '[ -r /etc/ra/tmux/zsh-autoattach.zsh ] && source /etc/ra/tmux/zsh-autoattach.zsh' \
  '# <<< ra tmux plugin: zsh auto-attach (managed) <<<' >> "${ZLOGIN}"
RA_INSTALLER_EOF
chmod 0755 "${INSTALLER}"

# 3. dpkg Post-Invoke: run the installer after every apt transaction. `|| true`
#    so it can never fail an apt operation. This is what catches the boot-time
#    zsh install regardless of startup ordering.
cat > /etc/apt/apt.conf.d/99-ra-tmux-zsh-autoattach <<RA_APT_EOF
DPkg::Post-Invoke { "${INSTALLER} || true"; };
RA_APT_EOF
chmod 0644 /etc/apt/apt.conf.d/99-ra-tmux-zsh-autoattach

# 4. Cover images that already have zsh at build time.
"${INSTALLER}" || true
