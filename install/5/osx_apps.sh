#!/usr/bin/env bash

echo ""
echo "--- Apple's license agreement"
echo "---- please accept 'sudo xcodebuild -license accept' by entering your password"
sudo xcodebuild -license accept


echo "--- Brew Cask for OSX apps"
brew tap caskroom/cask

# to search for more casks, visit http://caskroom.github.io/
apps=(
    alacritty
    appcleaner
    emacs
    docker
    dropbox
    google-chrome
    firefox
    flux
    gimp
    hammerspoon
    inkscape
    iterm2    
    keepassx
    spotify
    vagrant
    skype
    xquartz
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo ""
echo "--- homebrew cask apps:"
echo "${apps[@]}"
brew cask install --appdir="/Applications" ${apps[@]}
echo ""
echo ""
