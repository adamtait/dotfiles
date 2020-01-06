#!/usr/bin/env bash

echo ""
echo "--- Alacritty post-brew-cask-install"

START_DIR=`pwd`

cd $HOME/workspace

echo ""
echo "---- cloning github.com/jwilm/alacritty"
git clone git@github.com:jwilm/alacritty.git
cd alacritty

echo ""
echo "---- install Alacritty man pages"
sudo mkdir -p /usr/local/share/man/man1
gzip -c extra/alacritty.man | sudo tee /usr/local/share/man/man1/alacritty.1.gz > /dev/null


echo ""
echo "---- add Alacritty terminfo globally"
sudo tic -xe alacritty,alacritty-direct extra/alacritty.info

cd $START_DIR
