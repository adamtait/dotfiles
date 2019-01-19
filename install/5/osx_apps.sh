#!/usr/bin/env bash

echo ""
echo "--- Apple's license agreement"
echo "---- please accept 'sudo xcodebuild -license accept' by entering your password"
sudo xcodebuild -license accept


echo -e "\n--- Brew Cask for OSX apps"
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
  xquartz
  inkscape
  docker
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo  "\n--- homebrew cask apps:"
echo "${apps[@]}"
brew cask install --appdir="/Applications" ${apps[@]}
echo  "\n\n"
