#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


if [[ (-f $HOME/.gitconfig) && (! -h $HOME/.gitconfig) ]]; then
    echo -e "\n--- removing your old .gitconfig"
    rm $HOME/.gitconfig
fi

if [[ ! -h $HOME/.gitconfig ]]; then
    echo -e '\n--- creating a new .gitconfig'

    git_credential='cache'
    if [ "$(uname -s)" '==' "Darwin" ]
    then
        git_credential='osxkeychain'
    fi

    echo -e "\n\n"
    read -p ' - What is your github author name? ' git_authorname
    read -p ' - What is your github author email? ' git_authoremail

    emacs_client_script_path=$DOTFILES_DIR/bin/emacsclient

    sed \
        -e "s/AUTHORNAME/$git_authorname/g" \
        -e "s/AUTHOREMAIL/$git_authoremail/g" \
        -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" \
        -e "s#EMACSCLIENTSCRIPT#$emacs_client_script_path#g" \
        gitconfig.template > gitconfig.symlink

    ln -s $DOTFILES_DIR/gitconfig.symlink $HOME/.gitconfig
fi
