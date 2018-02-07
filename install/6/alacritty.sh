#!/usr/bin/env bash

echo -e "\n--- Alacritty (requires git & rustup already installed)"

START_DIR=`pwd`

if [[ ! -d $HOME/workspace ]]; then
    echo -e "\n--- creating ~/workspace directory"
    mkdir $HOME/workspace
fi

cd $HOME/workspace
git clone git@github.com:jwilm/alacritty.git
cd alacritty

echo -e "\n---- building Alacritty"
rustup override set stable
rustup update stable
cargo build --release
make app

echo -e "\n---- add Alacritty.app to /Applications"
cp -r target/release/osx/Alacritty.app /Applications/


cd $START_DIR
