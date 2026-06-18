# History — zsh translation of the old bash history.sh.

HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/history"
[[ -d "${HISTFILE:h}" ]] || mkdir -p "${HISTFILE:h}"

HISTSIZE=500000
SAVEHIST=500000

setopt APPEND_HISTORY          # append rather than overwrite
setopt INC_APPEND_HISTORY      # write each command as it runs
setopt SHARE_HISTORY           # share across live sessions
setopt HIST_IGNORE_ALL_DUPS    # erasedups
setopt HIST_IGNORE_SPACE       # ignore commands starting with a space
setopt HIST_REDUCE_BLANKS
setopt EXTENDED_HISTORY        # record timestamps (ISO-ish)

# Don't record noise (analog of HISTIGNORE).
HISTORY_IGNORE='(ls|bg|fg|exit|clear|history|pwd)'

# Incremental history search bound to up/down (matches the old readline bindings).
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
