#!/bin/zsh

CURRENT_DIR=$PWD
DOTFILES_DIR=$0:a:h
cd $DOTFILES_DIR

echo "loading git submodules"
git submodule init
git submodule update

if [[ ! -d $HOME ]]; then
    echo "please set $HOME before proceeding"
    exit 1
fi

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

echo "creating elpa directory & installing elpa packages"
if [[ -d $HOME/.emacs.d ]]; then
    echo "removing old emacs configuration"
    # safety in the case that you had already installed .emacs configuration
    sudo rm -rf $HOME/.emacs.d
fi
if [[ ! -h $HOME/.emacs.d ]]; then
    ln -s $DOTFILES_DIR/.emacs.d $HOME/
fi

vared -p "Would you like to install emacs elpa packages? (yes[y] or no[n]): " -c install_elpa
if [[ ("$install_elpa" == "y") || ("$install_elpa" == "yes") ]]; then
    rm -rf $DOTFILES_DIR/.emacs.d/elpa

    if [[ ! `emacs -version` =~ "24" ]]; then
	if [[ -d /Applications/Emacs.app ]]; then
	    #alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
	    echo "using /Applications/Emacs.app until zsh.d/emacs alias is setup correctly"
	    /Applications/Emacs.app/Contents/MacOS/Emacs -nw --script "${DOTFILES_DIR}/install_elpa.el" #&>/dev/null 
	else
	    echo "please run ./freshen.sh to install XEmacs (& do lots of other things) before running setup.sh, again"
	    exit 1
	fi
    else
	$EMACS_EXEC --script "${DOTFILES_DIR}/install_elpa.el" #&>/dev/null 
    fi
    
    echo "\n\n"
    echo "DONE installing elpa packages"
fi

echo "creating .path"
cat > "$HOME/.path" <<EOF
/bin
/usr/bin
/sbin
/usr/sbin
/opt/X11/bin
/usr/texbin
$(brew --prefix coreutils)/libexec/gnubin
/usr/local/bin
/opt/local/bin
$JAVA_HOME/bin
$DOTFILES_DIR/bin
$DOTFILES_DIR/submodules/bazel/output
EOF

echo "creating .manpath"
cat > "$HOME/.manpath" <<EOF
/usr/local/opt/coreutils/libexec/gnuman
EOF


echo "setting up symlinks"
if [[ ! -h $HOME/.zshrc ]]; then
    ln -s $DOTFILES_DIR/zshrc.symlink $HOME/.zshrc
fi
if [[ ! -d $HOME/.zsh.d ]]; then
    mkdir $HOME/.zsh.d
    ln -s $DOTFILES_DIR/zsh.d/* $HOME/.zsh.d/
fi
if [[ ! -h $HOME/.emacs ]]; then
    ln -s $DOTFILES_DIR/emacs.symlink $HOME/.emacs
fi
if [[ ! -h $HOME/.lein ]]; then
    ln -s $DOTFILES_DIR/.lein $HOME/.lein
fi
if [[ ! -h $HOME/.mjolnir ]]; then
    ln -s $DOTFILES_DIR/.mjolnir $HOME/
fi

# gitconfig
if [[ (-f $HOME/.gitconfig) && (! -h $HOME/.gitconfig) ]]; then
    echo "removing your old .gitconfig"
    rm $HOME/.gitconfig
fi
if [[ ! -h $HOME/.gitconfig ]]; then
    echo 'creating a new .gitconfig'

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


echo "adding OSX settings"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder


# install fonts
if [[ ! -e $HOME/Library/Fonts/Anonymous\ Pro.ttf ]]; then
    echo "installing Anonymous Pro (emacs font)"
    cd /tmp
    curl http://www.marksimonson.com/assets/content/fonts/AnonymousPro-1.002.zip > /tmp/AnonymousPro-1.002.zip
    unzip /tmp/AnonymousPro-1.002.zip
    cp /tmp/AnonymousPro-1.002.001/*.ttf $HOME/Library/Fonts/
    rm -rf /tmp/*AnonymousPro*
fi

# compile & install Bazel
if [[ ! -f $DOTFILES_DIR/submodules/bazel/output/bazel ]]; then
    echo "compiling Bazel"
    sh $DOTFILES_DIR/submodules/bazel/compile.sh
fi


cd $CURRENT_DIR
