#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


if [[ ! -h $HOME/.bashrc ]]; then
    echo -e "\n--- overwriting .bashrc"
    ln -s $DOTFILES_DIR/bashrc.symlink $HOME/.bashrc
fi

if [[ ! -h $HOME/.bash_profile ]]; then
    echo -e "\n--- replacing .bash_profile"
    ln -s $DOTFILES_DIR/bashrc.symlink $HOME/.bash_profile
fi

if [[ ! -d $HOME/.bash.d ]]; then
    echo -e "\n--- create .bash.d directory"
    rm -f $HOME/.bash.d
    mkdir $HOME/.bash.d
fi

if [[ ! -h $HOME/.bash.d/aliases.sh ]]; then
    echo -e "\n--- linking .bash.d/*"
    ln -s $DOTFILES_DIR/bash.d/* $HOME/.bash.d/
fi

if [[ ! -h $HOME/.emacs ]]; then
    echo -e "\n--- adding .emacs"
    ln -s $DOTFILES_DIR/emacs.symlink $HOME/.emacs
fi

if [[ ! -h $HOME/.lein ]]; then
    echo -e "\n--- adding .lein"
    ln -s $DOTFILES_DIR/lein $HOME/.lein
fi

if [[ ! -d $HOME/.hammerspoon ]]; then
    echo -e "\n--- creating .hammerspoon directory"
    rm -f $HOME/.hammerspoon
    mkdir $HOME/.hammerspoon
fi

if [[ ! -h $HOME/.hammerspoon/init.lua ]]; then
    echo -e "\n--- adding .hammerspoon/*"
    ln -s $DOTFILES_DIR/hammerspoon/* $HOME/.hammerspoon/
fi

if [[ ! -h $HOME/.tmux.conf ]]; then
    echo -e "\n--- adding .tmux.conf"
    ln -s $DOTFILES_DIR/tmux.conf $HOME/.tmux.conf
fi

if [[ ! -d $HOME/.nvm ]]; then
    echo -e "\n--- creating .nvm directory"
    rm -f $HOME/.nvm
    mkdir $HOME/.nvm
fi
