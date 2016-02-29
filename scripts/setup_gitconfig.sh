#!/bin/zsh

DOTFILES_DIR=$0:a:h/..


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

    vared -p ' - What is your github author name? ' -c git_authorname
    vared -p ' - What is your github author email? ' -c git_authoremail

    sed -e "s/AUTHORNAME/$git_authorname/g" -e "s/AUTHOREMAIL/$git_authoremail/g" -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" gitconfig.template > gitconfig.symlink

    ln -s $DOTFILES_DIR/gitconfig.symlink $HOME/.gitconfig
fi
