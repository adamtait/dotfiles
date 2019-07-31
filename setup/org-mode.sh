#!/usr/bin/env bash

echo ""
echo "--- org-mode setup"

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
OLD_PWD=`pwd`
cd $DOTFILES_DIR/submodules/org-mode
make
cd $OLD_PWD
