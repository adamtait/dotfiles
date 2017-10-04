#!/bin/bash
#
# FRESHEN
# re-install (or install fresh) all the apps you like (where you = Adam)
# code & ideas are largely taken from http://lapwinglabs.com/blog/hacker-guide-to-setting-up-your-mac

# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew recipes
brew update

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils

# Install Bash 4
brew install bash && \
echo $(brew --prefix)/bin/bash | sudo tee -a /etc/shells && \
chsh -s $(brew --prefix)/bin/bash

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH


# install some apps

binaries=(
  webkit2png
  python
  tree
  ack
  git
  rbenv
  ruby-build
  ninja
  ios-sim
)

echo "installing binaries..."
brew install ${binaries[@]}

brew cleanup


# check for existing emacs install
if [[ ! `emacs -version` =~ "25" ]]; then
    echo "removing old emacs version"
    sudo rm /usr/bin/emacs
    sudo rm -rf /usr/share/emacs
fi



# Brew Cask for OSX apps
brew install caskroom/cask/brew-cask

# to search for more casks, visit http://caskroom.github.io/
apps=(
  appcleaner
  emacs
  dropbox
  google-chrome
  firefox
  spotify
  vagrant
  iterm2
  flux
  skype
  keepassx
  hugo            # static site generator - https://gohugo.io
  inkscape        # vector graphics tool
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps..."
brew cask install --appdir="/Applications" ${apps[@]}

# Python Setup
pip install --upgrade pip setuptools

# Mjolnir.app (window manager)
if [[ ! -d /Applications/Mjolnir.app ]]; then
    echo "Installing Mjolnir.app (window manager)"
    CURRENT_DIR=$PWD
    cd /tmp
    curl -LOv https://github.com/sdegutis/mjolnir/releases/download/0.4.3/Mjolnir-0.4.3.tgz
    tar -zxvf Mjolnir-0.4.3.tgz
    sudo mv Mjolnir.app /Applications/
    rm Mjolnir*
    cd $CURRENT_DIR
fi

echo "Installing Lua -> window manager scripting language"
brew install lua
if [[ ! -s /usr/local/bin/luarocks ]]; then
    echo "Installing LuaRocks; deployment tools for Lua modules"
    CURRENT_DIR=$PWD
    cd /tmp
    curl -LOv http://keplerproject.github.io/luarocks/releases/luarocks-2.4.1.tar.gz
    tar -zxvf luarocks-2.4.1.tar.gz
    cd luarocks-2.4.1
    ./configure
    sudo make bootstrap
    sudo luarocks install luasocket
    cd ..
    rm -rf luarocks*
    cd $CURRENT_DIR
fi



if [[ ! -d ~/.luarocks ]]; then
    mkdir ~/.luarocks
fi
echo 'rocks_servers = { "http://rocks.moonscript.org" }' >> /usr/local/etc/luarocks52/config-5.2.lua

echo "Installing Mjolnir extensions"
luarocks install mjolnir.hotkey
luarocks install mjolnir.application

echo "Installing Bazel.io (Google Build Tool) prerequesites"
brew install protobuf libarchive
echo "NOTE: for Bazel to work, you still need to install JDK 1.8"


echo "Installing Dart Lang"
brew tap dart-lang/dart
brew install dart



echo -e "\n--- freshen.sh --- Complete."
