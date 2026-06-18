# Python via pyenv.

export PYENV_ROOT="$HOME/.pyenv"
[[ -d "$PYENV_ROOT/bin" ]] && path=("$PYENV_ROOT/bin" $path)

if (( $+commands[pyenv] )); then
  eval "$(pyenv init - zsh)"
fi
