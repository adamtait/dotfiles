#!/bin/zsh

DOTFILES_DIR=$0:a:h/..

echo "\n--- cleaning up any old emacs installation"
if [[ -d $HOME/.emacs.d ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    sudo rm -rf $HOME/.emacs.d
fi
if [[ ! -h $HOME/.emacs.d ]]; then
    ln -s $DOTFILES_DIR/.emacs.d $HOME/
fi


echo "\n--- checking Emacs installation"
if [[ ! -d /Applications/Emacs.app ]]; then
    echo "XEmacs didn't get installed properly. Try running ./freshen.sh"
    exit 1
fi

EMACS_EXEC=emacs
if [[ `emacs -version` != "24" ]]; then
    EMACS_EXEC="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

    echo "Adding alias for command-line emacs"
    touch $DOTFILES_DIR/zsh.d/emacs
    echo "alias emacs=\"${EMACS_EXEC}\"" >> $DOTFILES_DIR/zsh.d/emacs
    echo ">> you might want to: alias emacs=\"${EMACS_EXEC}\""
fi

echo "\n--- installing Emacs packages"
vared -p "Would you like to install emacs elpa packages? (yes[y] or no[n]): " -c install_elpa
if [[ ("$install_elpa" == "y") || ("$install_elpa" == "yes") ]]; then
    rm -rf $DOTFILES_DIR/.emacs.d/elpa

    # $EMACS_EXEC
    /Applications/Emacs.app/Contents/MacOS/Emacs --script "${DOTFILES_DIR}/scripts/install_elpa.el" \
      >/tmp/emacs.install_elpa.stdout.log \
      2>/tmp/emacs.install_elpa.stderr.log

    echo "DONE installing Emacs packages"
fi


