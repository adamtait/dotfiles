#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
echo ""
echo "--> dotfiles dir: ${DOTFILES_DIR}"

echo ""
echo "--- cleaning up any old emacs installation"
if [[ -d $HOME/.emacs.d ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    rm -rf $HOME/.emacs.d
fi
#if [[ ! -h $HOME/.emacs.d ]]; then
#    ln -s $DOTFILES_DIR/emacs.d $HOME/.emacs.d
#fi


echo ""
echo "--- checking CLI Emacs installation"
if ! command -v emacs &> /dev/null; then
    echo "------------"
    echo "WARNING!! CLI Emacs is not installed or on the PATH."
    echo "Try running brew install emacs"
    echo "------------"
    echo ""
    exit 1
fi


echo ""
echo "--- installing Emacs packages"

rm -f /tmp/.emacs

# Remove old config directory/link if it exists
if [[ -e $HOME/.emacs.d || -L $HOME/.emacs.d ]]; then
    rm -rf $HOME/.emacs.d
fi

# Symlink dotfiles config directory to ~/.emacs.d
ln -s $DOTFILES_DIR/configuration/emacs.d $HOME/.emacs.d

/opt/homebrew/bin/emacs --script ~/.emacs.d/install-my-packages.el

echo "--> DONE installing Emacs packages"
