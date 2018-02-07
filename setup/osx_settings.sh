#!/usr/bin/env bash

echo -e "\n--- OSX SETTINGS"
echo "Finder - show all files, even the hidden ones"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder
