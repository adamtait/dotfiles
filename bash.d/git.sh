# helpers for git

echo .DS_Store >> ~/.gitignore_global
git config --global core.excludesfile ~/.gitignore_global

alias g="git"
alias gst="git status"
alias gb="git branch"
