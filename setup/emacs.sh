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


# replaced by bash/d/emacs.sh (I think)
#

#if [[ `emacs -version` != "25" ]]; then
#    EMACS_EXEC="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
#    SCRIPT_PATH=$DOTFILES_DIR/configuration/bash/d/emacs.sh
#    
#    echo ""
#    echo "--- Adding alias for command-line emacs"
#
#    touch $SCRIPT_PATH
#    echo "alias emacs=\"${EMACS_EXEC}\"" >> $SCRIPT_PATH
#
#    echo "--->> you might want to: alias emacs=\"${EMACS_EXEC}\""
#fi



echo ""
echo "--- installing Emacs packages"

rm -f /tmp/.emacs

# run ELPA script
/Applications/Emacs.app/Contents/MacOS/Emacs --script "${DOTFILES_DIR}/setup/install_elpa.el"

echo "--> DONE installing Emacs packages"
