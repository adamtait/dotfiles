#!/bin/sh

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

echo "installing binaries..."
brew install ${binaries[@]}
brew cleanup
