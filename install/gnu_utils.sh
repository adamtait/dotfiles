#!/usr/bin/env bash

echo -e "\n--- Install GNU core utilities (those that come with OS X are outdated)"
brew install coreutils

echo -e "\n--- Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed"
brew install findutils
