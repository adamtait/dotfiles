# Node.js via fnm (fast, single binary, cross-platform). Replaces nvm + ~/.nvm.

if (( $+commands[fnm] )); then
  eval "$(fnm env --use-on-cd --shell zsh)"
fi
