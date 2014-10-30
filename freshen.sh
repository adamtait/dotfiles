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
brew install bash

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep

$PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH


# install some apps

binaries=(
  webkit2png
  python
  tree
  ack
  git
)

echo "installing binaries..."
brew install ${binaries[@]}

brew cleanup


# Brew Cask for OSX apps
brew install caskroom/cask/brew-cask

# to search for more casks, visit http://caskroom.io/
apps=(
  emacs
  dropbox
  google-chrome
  appcleaner
  firefox
  spotify
  vagrant
  iterm2
  flux
  skype
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps..."
brew cask install --appdir="/Applications" ${apps[@]}


# Oh-My-Zsh
if [[ ! -d ~/.oh-my-zsh ]]; then
    git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
    chsh -s /bin/zsh
fi

# Mjolnir.app (window manager)
if [[ ! -d /Applications/Mjolnir.app ]]; then
    echo "Installing Mjolnir.app (window manager)"
    CURRENT_DIR=$PWD
    cd /tmp
    curl -LOv https://github.com/sdegutis/mjolnir/releases/download/0.4.3/Mjolnir-0.4.3.tgz
    tar -zxvf /tmp/Mjolnir-0.4.3.tgz /tmp/
    mv Mjolnir.app /Applications/
    rm Mjolnir*
    cd $CURRENT_DIR
fi

echo "Installing Lua -> window manager scripting language"
brew install lua
brew install luarocks

if [[ ! -d ~/.luarocks ]]; then
    mkdir ~/.luarocks
fi
echo 'rocks_servers = { "http://rocks.moonscript.org" }' > ~/.luarocks/config.lua

echo "Installing Mjolnir extensions"
luarocks install mjolnir.hotkey
luarocks install mjolnir.application

