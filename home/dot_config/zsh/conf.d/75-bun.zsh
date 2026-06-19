# Bun (JavaScript runtime + package manager). Installed via the official installer
# (run_once_after_23-install-bun). `bun install -g` bins also land in $BUN_INSTALL/bin,
# so this is what puts globally-installed CLIs (e.g. qmd) on PATH.
# (path stays de-duplicated automatically — 00-path.zsh declared `typeset -U path`.)

export BUN_INSTALL="$HOME/.bun"
[[ -d "$BUN_INSTALL/bin" ]] && path=("$BUN_INSTALL/bin" $path)
[[ -s "$BUN_INSTALL/_bun" ]] && source "$BUN_INSTALL/_bun"   # completions
