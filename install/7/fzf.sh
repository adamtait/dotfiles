#!/usr/bin/env bash

echo ""
echo "--- fzf (command line fuzzy finder)"
brew install fzf

echo ""
echo "----> SETUP for fzf"
echo "Recommended answers:  [y, y, n]"
echo "shell configuration files are already updated for fzf.\n"
$(brew --prefix)/opt/fzf/install
