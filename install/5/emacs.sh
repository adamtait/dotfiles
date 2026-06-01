#!/usr/bin/env bash


# check for existing emacs install
if ! command -v emacs &> /dev/null; then
    echo ""
    echo "--- installing native CLI emacs"
    brew install emacs
fi
