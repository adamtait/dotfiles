#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
KB_FILE=DefaultKeyBinding.dict
KB_PATH=$HOME/Library/KeyBindings/$KB_FILE

if [[ ! -h $KB_PATH ]]; then
    echo -e '\n--- symlink $KB_FILE'
    ln -s $DOTFILES_DIR/configuration/osx/$KB_FILE $KB_PATH
fi
