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
    ln -s $PWD/.emacs.d ~/
fi

vared -p "Would you like to install emacs elpa packages? (yes[y] or no[n]): " -c install_elpa
if [[ ("$install_elpa" == "y") || ("$install_elpa" == "yes") ]]; then
    rm -rf $PWD/.emacs.d/elpa
    emacs --script "$PWD/install_elpa.el" &>/dev/null 
    echo "\n\n"
    echo "DONE installing elpa packages"
fi

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
$(brew --prefix coreutils)/libexec/gnubin
$JAVA_HOME/bin
$PWD/submodules/bazel/output
EOF

echo "creating .manpath"
cat > "$HOME/.manpath" <<EOF
/usr/local/opt/coreutils/libexec/gnuman
EOF


echo "setting up symlinks"
if [[ ! -h ~/.zshrc ]]; then
    ln -s $PWD/zshrc.symlink ~/.zshrc
fi
if [[ ! -d ~/.zsh.d ]]; then
    mkdir ~/.zsh.d
    ln -s $PWD/zsh.d/* ~/.zsh.d/
fi
if [[ ! -h ~/.emacs ]]; then
    ln -s $PWD/emacs.symlink ~/.emacs
fi
if [[ ! -h ~/.lein ]]; then
    ln -s $PWD/.lein ~/.lein
fi
if [[ ! -h ~/.mjolnir ]]; then
    ln -s $PWD/.mjolnir ~/
fi
if [[ ! -h ~/.gitconfig ]]; then
    ln -s $PWD/.gitconfig ~/
fi

# gitconfig
if [[ (-f ~/.gitconfig) && (! -h ~/.gitconfig) ]]; then
    echo "removing your old .gitconfig"
    rm ~/.gitconfig
fi
if [[ ! -h ~/.gitconfig ]]; then
    echo 'creating a new .gitconfig'

    git_credential='cache'
    if [ "$(uname -s)" '==' "Darwin" ]
    then
        git_credential='osxkeychain'
    fi

    echo ' - What is your github author name?'
    read -e git_authorname
    echo ' - What is your github author email?'
    read -e git_authoremail

    sed -e "s/AUTHORNAME/$git_authorname/g" -e "s/AUTHOREMAIL/$git_authoremail/g" -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" gitconfig.template > gitconfig.symlink

    ln -s $PWD/gitconfig.symlink ~/.gitconfig
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

# compile & install Bazel
if [[ ! -f $PWD/submodules/bazel/output/bazel ]]; then
    echo "compiling Bazel"
    sh $PWD/submodules/bazel/compile.sh
fi


cd $CURRENT_DIR
