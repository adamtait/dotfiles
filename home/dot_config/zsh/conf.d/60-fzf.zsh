# fzf — key bindings + completion (cross-platform: brew installs ship a shell-integration
# script; recent fzf also supports `fzf --zsh`).

if (( $+commands[fzf] )); then
  if fzf --zsh >/dev/null 2>&1; then
    source <(fzf --zsh)
  else
    # Older fzf: source brew-installed integration files if present (reuse the prefix
    # exported by `brew shellenv`, no extra fork).
    if [[ -n "${HOMEBREW_PREFIX:-}" ]]; then
      _fzf_base="$HOMEBREW_PREFIX/opt/fzf/shell"
      [[ -f "$_fzf_base/key-bindings.zsh" ]] && source "$_fzf_base/key-bindings.zsh"
      [[ -f "$_fzf_base/completion.zsh" ]] && source "$_fzf_base/completion.zsh"
      unset _fzf_base
    fi
  fi

  # Prefer fd for traversal when available.
  if (( $+commands[fd] )); then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  fi
fi
