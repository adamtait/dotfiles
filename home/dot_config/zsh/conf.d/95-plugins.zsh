# Third-party zsh plugins fetched via chezmoi externals (.chezmoiexternal.toml).
# Syntax highlighting must be sourced last to wrap the line editor correctly.

ZSH_PLUGIN_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh/plugins"

if [[ -f "$ZSH_PLUGIN_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
  source "$ZSH_PLUGIN_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"
  # Ctrl-Space accepts the current suggestion and runs it in one keystroke.
  # (The autosuggest-execute widget is registered when the plugin sources.)
  bindkey '^ ' autosuggest-execute
fi

# Keep syntax-highlighting last.
[[ -f "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && \
  source "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
