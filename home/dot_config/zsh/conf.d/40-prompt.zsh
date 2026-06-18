# Prompt — Starship (single static binary, identical on macOS + Linux, OS/git/runtime
# aware on its own so no runtime `uname` branching is needed). Replaces bash-powerline.sh.

export STARSHIP_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/starship.toml"

if (( $+commands[starship] )); then
  eval "$(starship init zsh)"
else
  # Minimal fallback prompt if starship isn't installed yet (e.g. mid-bootstrap).
  autoload -Uz vcs_info
  zstyle ':vcs_info:git:*' formats ' (%b)'
  precmd() { vcs_info }
  setopt PROMPT_SUBST
  PROMPT='%F{cyan}%~%f%F{yellow}${vcs_info_msg_0_}%f %# '
fi
