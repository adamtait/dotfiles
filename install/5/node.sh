#! /usr/local/bin/bash

echo -e "\n--- NVM (Node Version Manager)"
brew install nvm


echo -e "\n----- Node LTS versions"
source ../bash.d/nvm.sh
nvm install --lts=BORON   # node v6.12.3 (npm v3.10.10)
nvm install --lts
