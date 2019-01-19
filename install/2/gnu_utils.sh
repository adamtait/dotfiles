#!/usr/bin/env bash

echo ""
echo "--- Install GNU core utilities (those that come with OS X are outdated)"
brew install coreutils

echo ""
echo "--- Install GNU find, locate, updatedb, and xargs, g-prefixed"
brew install findutils
