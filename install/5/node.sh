#! /usr/local/bin/bash

echo ""
echo "--- NVM (Node Version Manager)"
brew install nvm


echo ""
echo "----- Node LTS versions"
source ../../configuration/bash/d/nvm.sh
nvm install --lts=BORON   # node v6.12.3 (npm v3.10.10)
nvm install --lts
