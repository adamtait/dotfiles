#!/usr/bin/env bash

echo ""
echo "--- create ~/workspace directory"

if [[ ! -d $HOME/workspace ]]; then
    echo ""
    echo "--- creating ~/workspace directory"
    mkdir $HOME/workspace
fi
