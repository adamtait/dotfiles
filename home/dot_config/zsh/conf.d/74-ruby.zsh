# Ruby via rbenv.

export RBENV_ROOT="$HOME/.rbenv"
[[ -d "$RBENV_ROOT/bin" ]] && path=("$RBENV_ROOT/bin" $path)

if (( $+commands[rbenv] )); then
  eval "$(rbenv init - zsh)"
fi
