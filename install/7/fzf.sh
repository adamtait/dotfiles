#!/usr/bin/env bash

echo -e "\n--- fzf (command line fuzzy finder)"
brew install fzf

echo -e "\n----> SETUP for fzf"
echo "Recommended answers:  [y, y, n]"
echo "shell configuration files are already updated for fzf.\n"
$(brew --prefix)/opt/fzf/install
