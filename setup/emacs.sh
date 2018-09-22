#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
echo -e "\n--> dotfiles dir: ${DOTFILES_DIR}"

echo -e "\n--- cleaning up any old emacs installation"
if [[ -d $HOME/.emacs.d ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    sudo rm -rf $HOME/.emacs.d
fi
if [[ ! -h $HOME/.emacs.d ]]; then
    ln -s $DOTFILES_DIR/emacs.d $HOME/.emacs.d
fi


echo -e "\n--- checking Emacs installation"
if [[ ! -d /Applications/Emacs.app ]]; then
    echo "XEmacs didn't get installed properly. Try running ./freshen.sh"
    exit 1
fi


if [[ `emacs -version` != "25" ]]; then
    EMACS_EXEC="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
    SCRIPT_PATH=$DOTFILES_DIR/configuration/bash/d/emacs.sh
    
    echo -e "\n\n--- Adding alias for command-line emacs"

    touch $SCRIPT_PATH
    echo "alias emacs=\"${EMACS_EXEC}\"" >> $SCRIPT_PATH

    echo "--->> you might want to: alias emacs=\"${EMACS_EXEC}\""
fi



echo -e "\n--- installing Emacs packages"
function install_elpa {
    rm -rf $DOTFILES_DIR/emacs/d/elpa

    # run ELPA script
    /Applications/Emacs.app/Contents/MacOS/Emacs --script "${DOTFILES_DIR}/setup/install_elpa.el" \
      >/tmp/emacs.install_elpa.stdout.log \
      2>/tmp/emacs.install_elpa.stderr.log
    echo "--> DONE installing Emacs packages"
}

echo "Would you like to install emacs elpa packages? ('yes' or 'no'): "
select user_choice in "yes" "no"; do
    case $user_choice in
        yes ) install_elpa; break;;
        no ) exit;;
    esac
done
