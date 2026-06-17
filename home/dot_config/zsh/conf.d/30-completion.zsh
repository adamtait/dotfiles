# Completion — zsh's native compinit (replaces the 71KB vendored bash_completion blob).

# Cache compdump under XDG to keep $HOME clean.
ZSH_COMPDUMP="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump"
[[ -d "${ZSH_COMPDUMP:h}" ]] || mkdir -p "${ZSH_COMPDUMP:h}"

autoload -Uz compinit
compinit -d "$ZSH_COMPDUMP"

# Case-insensitive, hyphen/underscore-insensitive matching (matches old readline settings).
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}'
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' show-all-if-ambiguous true

# Homebrew-provided completions (macOS + linuxbrew).
if (( $+commands[brew] )); then
  fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fi
