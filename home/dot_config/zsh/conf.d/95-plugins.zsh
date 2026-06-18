# Third-party zsh plugins fetched via chezmoi externals (.chezmoiexternal.toml).
# Syntax highlighting must be sourced last to wrap the line editor correctly.

ZSH_PLUGIN_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh/plugins"

[[ -f "$ZSH_PLUGIN_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] && \
  source "$ZSH_PLUGIN_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"

# Keep syntax-highlighting last.
[[ -f "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && \
  source "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
