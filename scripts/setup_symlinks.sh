#!/bin/zsh

DOTFILES_DIR=$0:a:h/..


if [[ ! -h $HOME/.zshrc ]]; then
    echo "--- adding .zshrc"
    ln -s $DOTFILES_DIR/zshrc.symlink $HOME/.zshrc
fi

if [[ ! -d $HOME/.zsh.d ]]; then
    echo "--- adding .zsh.d"
    mkdir $HOME/.zsh.d
    ln -s $DOTFILES_DIR/zsh.d/* $HOME/.zsh.d/
fi

if [[ ! -h $HOME/.emacs ]]; then
    echo "--- adding .emacs"
    ln -s $DOTFILES_DIR/emacs.symlink $HOME/.emacs
fi

if [[ ! -h $HOME/.lein ]]; then
    echo "--- adding .lein"
    ln -s $DOTFILES_DIR/.lein $HOME/.lein
fi

if [[ ! -h $HOME/.mjolnir ]]; then
    echo "--- adding .mjolnir"
    ln -s $DOTFILES_DIR/.mjolnir $HOME/
fi
