#!/usr/bin/env bash

echo ""
echo "--- OSX SETTINGS"
echo "Finder - show all files, even the hidden ones"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder
