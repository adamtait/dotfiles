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
  wget
  rbenv
  ruby-build
  ninja
  ios-sim
  hugo            # static site generator - https://gohugo.io
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
brew tap caskroom/cask

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
  hammerspoon
  flux
  skype
  keepassx
  xquartz         # pre-requisite for Inkscape
  inkscape        # vector graphics tool
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps..."
brew cask install --appdir="/Applications" ${apps[@]}¯




# Python lang
pip install --upgrade pip setuptools


# Ruby lang
echo "Installing Ruby 2.5.0"
rbenv install 2.5.0
rbenv global 2.5.0

# Cocoapods
echo "Installing Cocoapods"
gem install cocoapods



# Dart lang
echo "Installing Dart Lang"
brew tap dart-lang/dart
brew install dart

# Bazel build tool
echo "Installing Bazel.io (Google Build Tool) prerequesites"
brew install protobuf libarchive
echo "NOTE: for Bazel to work, you still need to install JDK 1.8"




echo -e "\n--- freshen.sh --- Complete."
