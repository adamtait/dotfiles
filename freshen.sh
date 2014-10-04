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

apps=(
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
