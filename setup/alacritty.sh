#!/usr/bin/env bash

echo ""
echo "---- add Alacritty config file"

if [[ ! -d $HOME/.config ]]; then
    echo "----- creating ~/.config directory"
    mkdir $HOME/.config
fi


DOTFILES_DIR=$(dirname $(realpath "$0"))/..

if [[ ! -h $HOME/.config/alacritty ]]; then
    echo "----- adding symlink for .config/alacritty"
    ln -s $DOTFILES_DIR/configuration/alacritty $HOME/.config/alacritty
fi
