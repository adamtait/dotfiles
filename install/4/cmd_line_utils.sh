#!/usr/bin/env bash

# install command line utils (via Homebrew)

binaries=(
    ack
    borkdude/brew/babashka
    git
    hugo
    ios-sim
    ninja
    pipx
    python
    rbenv
    ripgrep
    ruby-build
    tree
    tmux
    watchman
    webkit2png
    wget
)

echo ""
echo "---  Homebrew binaries installing: "
echo "${binaries[@]}"

brew install ${binaries[@]}
brew cleanup

echo ""
echo ""
