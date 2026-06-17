# General interactive options.

setopt AUTO_CD                 # `foo` == `cd foo` when foo is a dir
setopt AUTO_PUSHD             # cd pushes onto the dir stack
setopt PUSHD_IGNORE_DUPS
setopt EXTENDED_GLOB
setopt INTERACTIVE_COMMENTS   # allow `# comments` in interactive shells
setopt NO_BEEP

# Sensible word boundaries for ^W etc.
autoload -Uz select-word-style && select-word-style bash
