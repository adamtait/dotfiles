#!/usr/bin/env bash

echo ""
echo "--- Apple's license agreement"
echo "---- please accept 'sudo xcodebuild -license accept' by entering your password"
sudo xcodebuild -license accept


echo "--- Brew Cask for OSX apps"
brew tap homebrew/cask

# to search for more casks, visit http://caskroom.github.io/
apps=(
    appcleaner
    emacs
    google-chrome
    firefox
    gimp
    hammerspoon
    inkscape
    iterm2    
    keepassxc
    spotify
    xquartz
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo ""
echo "--- homebrew cask apps:"
echo "${apps[@]}"
brew install --cask ${apps[@]}
echo ""
echo ""
