# Completion — zsh's native compinit (replaces the 71KB vendored bash_completion blob).

# Cache compdump under XDG to keep $HOME clean.
ZSH_COMPDUMP="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump"
[[ -d "${ZSH_COMPDUMP:h}" ]] || mkdir -p "${ZSH_COMPDUMP:h}"

# Homebrew-provided completions (macOS + linuxbrew) must be on fpath BEFORE compinit,
# otherwise compinit never sees them. Reuse the prefix exported by `brew shellenv` in
# 00-path.zsh rather than forking `brew --prefix` on every shell start.
if [[ -n "${HOMEBREW_PREFIX:-}" && -d "$HOMEBREW_PREFIX/share/zsh/site-functions" ]]; then
  fpath=("$HOMEBREW_PREFIX/share/zsh/site-functions" $fpath)
fi

autoload -Uz compinit
compinit -d "$ZSH_COMPDUMP"

# Case-insensitive, hyphen/underscore-insensitive matching (matches old readline settings).
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}'
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' show-all-if-ambiguous true
