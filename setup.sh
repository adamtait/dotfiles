#!/bin/zsh

CURRENT_DIR=$PWD

echo "loading git submodules"
git submodule init
git submodule update

echo "creating elpa directory & installing elpa packages"
if [[ ! -d $PWD/.emacs.d/elpa ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    sudo rm -rf ~/.emacs.d
fi
if [[ ! -h ~/.emacs.d ]]; then
    ln -s $PWD/emacs.d ~/
fi
rm -rf $PWD/.emacs.d/elpa
emacs --script "$PWD/install_elpa.el"


echo "creating .path"
    cat > "$HOME/.path" <<EOF
$HOME/.dotfiles/bin
/usr/local/bin
/opt/local/bin
/usr/bin
/bin
/usr/sbin
/sbin
/opt/X11/bin
/usr/texbin
$JAVA_HOME/bin
EOF


echo "setting up symlinks"
if [[ ! -h ~/.zshrc ]]; then
    ln -s $PWD/zshrc.symlink ~/.zshrc
fi
if [[ ! -h ~/.emacs ]]; then
    ln -s $PWD/emacs.symlink ~/.emacs
fi
if [[ ! -h ~/.mjolnir ]]; then
    ln -s $PWD/.mjolnir ~/
fi
if [[ ! -h ~/.gitconfig ]]; then
    ln -s $PWD/.gitconfig ~/
fi



# gitconfig
if [[ -f ~/.gitconfig ]]; then
    echo "removing your old .gitconfig"
    rm ~/.gitconfig
fi
if [[ ! -h ~/gitconfig.symlink ]]; then
    echo 'creating a new gitconfig'

    git_credential='cache'
    if [ "$(uname -s)" == "Darwin" ]
    then
        git_credential='osxkeychain'
    fi

    echo ' - What is your github author name?'
    read -e git_authorname
    echo ' - What is your github author email?'
    read -e git_authoremail

    sed -e "s/AUTHORNAME/$git_authorname/g" -e "s/AUTHOREMAIL/$git_authoremail/g" -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" gitconfig.symlink > gitconfig.symlink

    ln -s gitconfig.symlink ~/.gitconfig
fi


echo "adding OSX settings"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder


# install fonts
if [[ ! -e ~/Library/Fonts/Anonymous\ Pro.ttf ]]; then
    echo "installing Anonymous Pro (emacs font)"
    cd /tmp
    curl http://www.marksimonson.com/assets/content/fonts/AnonymousPro-1.002.zip > /tmp/AnonymousPro-1.002.zip
    unzip /tmp/AnonymousPro-1.002.zip
    cp /tmp/AnonymousPro-1.002.001/*.ttf ~/Library/Fonts/
    rm -rf /tmp/*AnonymousPro*
fi

cd $CURRENT_DIR
