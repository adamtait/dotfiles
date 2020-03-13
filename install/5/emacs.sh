#!/usr/bin/env bash


# check for existing emacs install
if [[ ! `emacs -version` =~ "26" ]]; then
    echo ""
    echo "--- removing old emacs version"
    sudo rm -f /usr/bin/emacs
    sudo rm -rf /usr/share/emacs
    brew install emacs
    brew services start emacs
fi
