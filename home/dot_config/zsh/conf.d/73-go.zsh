# Go.

export GOPATH="$HOME/go"
# `go` itself is installed onto PATH by the package step; this adds `go install`ed bins.
# (path stays de-duplicated automatically — 00-path.zsh declared `typeset -U path`.)
path=("$GOPATH/bin" $path)
