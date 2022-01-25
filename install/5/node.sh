#! /usr/bin/env bash

echo ""
echo "--- NVM (Node Version Manager)"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash


echo ""
echo "----- Node LTS versions"
source ../../configuration/bash/d/nvm.sh
nvm install --lts
