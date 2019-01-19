#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
KB_FILE=DefaultKeyBinding.dict
KB_PATH=$HOME/Library/KeyBindings

if [[ ! -d $KB_PATH ]]; then
   echo ""
   echo "--- creating $KB_PATH"
   mkdir $KB_PATH
fi

if [[ ! -h $KB_PATH ]]; then
    echo ""
    echo "--- symlink $KB_FILE"
    ln -s $DOTFILES_DIR/configuration/osx/$KB_FILE $KB_PATH/
fi
