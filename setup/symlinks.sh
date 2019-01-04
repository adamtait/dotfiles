#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
CONFIG_DIR=$DOTFILES_DIR/configuration



if [[ ! -h $HOME/.bashrc ]]; then
    echo ""
    echo "--- overwriting .bashrc"
    ln -s $CONFIG_DIR/bash/bashrc $HOME/.bashrc
fi

if [[ ! -h $HOME/.bash_profile ]]; then
    echo ""
    echo "--- replacing .bash_profile"
    ln -s $CONFIG_DIR/bash/bashrc $HOME/.bash_profile
fi

if [[ ! -d $HOME/.bash.d ]]; then
    echo ""
    echo "--- create .bash.d directory"
    rm -f $HOME/.bash.d
    mkdir $HOME/.bash.d
fi

if [[ ! -h $HOME/.bash.d/aliases.sh ]]; then
    echo ""
    echo "--- linking .bash.d/*"
    ln -s $CONFIG_DIR/bash/d/* $HOME/.bash.d/
fi

if [[ ! -h $HOME/.emacs ]]; then
    echo ""
    echo "--- adding .emacs"
    ln -s $CONFIG_DIR/emacs/emacs $HOME/.emacs
fi

if [[ ! -h $HOME/.lein ]]; then
    echo ""
    echo "--- adding .lein"
    ln -s $CONFIG_DIR/lein $HOME/.lein
fi

if [[ ! -d $HOME/.hammerspoon ]]; then
    echo ""
    echo "--- creating .hammerspoon directory"
    rm -f $HOME/.hammerspoon
    mkdir $HOME/.hammerspoon
fi

if [[ ! -h $HOME/.hammerspoon/init.lua ]]; then
    echo ""
    echo "--- adding .hammerspoon/*"
    ln -s $CONFIG_DIR/hammerspoon/* $HOME/.hammerspoon/
fi

if [[ ! -h $HOME/.tmux.conf ]]; then
    echo ""
    echo "--- adding .tmux.conf"
    ln -s $CONFIG_DIR/tmux/tmux.conf $HOME/.tmux.conf
fi

if [[ ! -d $HOME/.nvm ]]; then
    echo ""
    echo "--- creating .nvm directory"
    rm -f $HOME/.nvm
    mkdir $HOME/.nvm
fi
