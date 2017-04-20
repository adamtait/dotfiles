#!/bin/sh

DOTFILES_DIR=$(dirname "$0")/..


if [[ (-f $HOME/.gitconfig) && (! -h $HOME/.gitconfig) ]]; then
    echo "--- removing your old .gitconfig"
    rm $HOME/.gitconfig
fi

if [[ ! -h $HOME/.gitconfig ]]; then
    echo '--- creating a new .gitconfig'

    git_credential='cache'
    if [ "$(uname -s)" '==' "Darwin" ]
    then
        git_credential='osxkeychain'
    fi

    read -p ' - What is your github author name? ' git_authorname
    read -p ' - What is your github author email? ' git_authoremail

    sed -e "s/AUTHORNAME/$git_authorname/g" -e "s/AUTHOREMAIL/$git_authoremail/g" -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" gitconfig.template > gitconfig.symlink

    ln -s $DOTFILES_DIR/gitconfig.symlink $HOME/.gitconfig
fi
