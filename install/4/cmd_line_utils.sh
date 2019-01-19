#!/usr/bin/env bash

# install command line utils (via Homebrew)

binaries=(
  webkit2png
  python
  tree
  ack
  git
  tmux
  wget
  rbenv
  ruby-build
  ninja
  ios-sim
  hugo            # static site generator - https://gohugo.io
  watchman
)

echo ""
echo "---  Homebrew binaries installing: "
echo "${binaries[@]}"

brew install ${binaries[@]}
brew cleanup

echo ""
echo ""
