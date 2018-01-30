#!/bin/sh

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


if [[ ! -h $HOME/.bashrc ]]; then
    echo "--- overwriting .bashrc"
    ln -s $DOTFILES_DIR/bashrc.symlink $HOME/.bashrc
fi

if [[ ! -h $HOME/.bash_profile ]]; then
    echo "--- replacing .bash_profile"
    ln -s $DOTFILES_DIR/bashrc.symlink $HOME/.bash_profile
fi

if [[ ! -d $HOME/.bash.d ]]; then
    echo "--- create .bash.d directory"
    rm -f $HOME/.bash.d
    mkdir $HOME/.bash.d
fi

if [[ ! -h $HOME/.bash.d/aliases.sh ]]; then
    echo "--- linking .bash.d/*"
    ln -s $DOTFILES_DIR/bash.d/* $HOME/.bash.d/
fi

if [[ ! -h $HOME/.emacs ]]; then
    echo "--- adding .emacs"
    ln -s $DOTFILES_DIR/emacs.symlink $HOME/.emacs
fi

if [[ ! -h $HOME/.lein ]]; then
    echo "--- adding .lein"
    ln -s $DOTFILES_DIR/lein $HOME/.lein
fi

if [[ ! -d $HOME/.hammerspoon ]]; then
    echo "--- creating .hammerspoon directory"
    rm -f $HOME/.hammerspoon
    mkdir $HOME/.hammerspoon
fi

if [[ ! -h $HOME/.hammerspoon/init.lua ]]; then
    echo "--- adding .hammerspoon/*"
    ln -s $DOTFILES_DIR/hammerspoon/* $HOME/.hammerspoon/
fi

if [[ ! -h $HOME/.tmux.conf ]]; then
    echo "--- adding .tmux.conf"
    ln -s $DOTFILES_DIR/tmux.conf $HOME/.tmux.conf
fi

