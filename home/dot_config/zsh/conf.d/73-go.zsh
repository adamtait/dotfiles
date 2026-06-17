# Go.

export GOPATH="$HOME/go"
path=("$GOPATH/bin" $path)
# Homebrew/apt install `go` onto PATH already; this just adds installed binaries.
typeset -U path
