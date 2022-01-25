#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
echo ""
echo "--> dotfiles dir: ${DOTFILES_DIR}"

echo ""
echo "--- cleaning up any old emacs installation"
if [[ -d $HOME/.emacs.d ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    sudo rm -rf $HOME/.emacs.d
fi
#if [[ ! -h $HOME/.emacs.d ]]; then
#    ln -s $DOTFILES_DIR/emacs.d $HOME/.emacs.d
#fi


echo ""
echo "--- checking Emacs installation"
if [[ ! -d /Applications/Emacs.app ]]; then
    echo "------------"
    echo "WARNING!! XEmacs didn't get installed properly."
    echo "Try running .dotfiles/install.sh"
    echo "------------"
    echo ""
    exit 1
fi


echo ""
echo "--- installing Emacs packages"

rm -f /tmp/.emacs
sudo ln -sf /opt/homebrew/Cellar/emacs/27.2/bin/emacs* /usr/local/bin/
cp -r ~/.dotfiles/configuration/emacs.d ~/.emacs.d
/usr/local/bin/emacs --script ~/.emacs.d/install-my-packages.el

echo "--> DONE installing Emacs packages"
