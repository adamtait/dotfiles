#!/bin/sh

echo "--- Node.js & NPM"

brew install nvm
nvm install --lts=BORON   # node v6.12.3 (npm v3.10.10)
nvm install --lts


echo "--- React Native components"
npm install -g react-native-cli

