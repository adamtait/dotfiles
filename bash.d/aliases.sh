#!/usr/bin/env bash

# alias to search processes
alias pgrep='ps -efa | grep'

# alias to show the date
alias da='date "+%Y-%m-%d %A %T %Z"'


# Alias's to modified commands
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -iv'
alias mkdir='mkdir -p'
alias ps='ps auxf'
alias ping='ping -c 10'
alias less='less -R'
alias cls='clear'

# Change directory aliases
alias home='cd ~'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# cd into the old directory
alias bd='cd "$OLDPWD"'

# Remove a directory and all files
alias rmd='/bin/rm  --recursive --force --verbose '

# Alias's for multiple directory listing commands
alias l='ls -1F'
alias ll='ls -lA'
alias la='ls -Alh' # show hidden files
#alias ls='ls -aFh --color=always' # add colors and file type extensions
alias lx='ls -lXBh' # sort by extension
alias lk='ls -lSrh' # sort by size
alias lc='ls -lcrh' # sort by change time
alias lu='ls -lurh' # sort by access time
alias lr='ls -lRh' # recursive ls
alias lt='ls -ltrh' # sort by date
alias lm='ls -alh |more' # pipe through 'more'
alias lw='ls -xAh' # wide listing format
alias ll='ls -Fls' # long listing format
alias labc='ls -lap' #alphabetical sort
alias lf="ls -l | egrep -v '^d'" # files only
alias ld="ls -l | egrep '^d'" # directories only
alias ldir='ld'


# Search command line history
alias h="history | grep "


# just for fun
alias d="cd ~/.dotfiles"


# IP addresses
alias publicip='dig +short myip.opendns.com @resolver1.opendns.com'
alias wanip='publicip'
alias myip="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'"
alias localip='myip'
alias ip='myip'


# Time
alias epochtime='date +%s'




# from: https://remysharp.com/2018/08/23/cli-improved

alias cat='bat'
alias ping='prettyping --nolegend'
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias top="htop"
alias find="fd"
alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias help="tldr"
