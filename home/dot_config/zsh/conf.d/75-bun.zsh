# Bun (JavaScript runtime + package manager). Installed via the official installer
# (run_once_after_23-install-bun). $BUN_INSTALL/bin (bun itself + `bun install -g` CLIs like
# qmd) is added to PATH declaratively via ~/.local/path (see 00-path.zsh); here we just set
# $BUN_INSTALL and load completions.

export BUN_INSTALL="$HOME/.bun"
[[ -s "$BUN_INSTALL/_bun" ]] && source "$BUN_INSTALL/_bun"   # completions
