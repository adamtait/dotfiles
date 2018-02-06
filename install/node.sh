#! /usr/local/bin/bash

echo -e "\n--- NVM (Node Version Manager)"
brew install nvm


echo -e "\n----- Node LTS versions"
source ../bash.d/nvm.sh
nvm install --lts
nvm install --lts=BORON   # node v6.12.3 (npm v3.10.10)


echo -e "\n----- NPM React Native components"
npm install -g react-native-cli

nvm use --lts
