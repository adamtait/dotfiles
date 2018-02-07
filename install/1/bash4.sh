#!/usr/bin/env bash

# Install Bash 4
echo -e "\n--- bash 4+"
brew install bash && \
echo $(brew --prefix)/bin/bash | sudo tee -a /etc/shells && \
chsh -s $(brew --prefix)/bin/bash
