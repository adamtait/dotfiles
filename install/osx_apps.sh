#!/bin/sh

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

