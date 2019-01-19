#!/usr/bin/env bash

echo ""
echo "--- Alacritty (requires git & rustup already installed)"

START_DIR=`pwd`

if [[ ! -d $HOME/workspace ]]; then
    echo ""
    echo "--- creating ~/workspace directory"
    mkdir $HOME/workspace
fi

cd $HOME/workspace
git clone git@github.com:jwilm/alacritty.git
cd alacritty

echo ""
echo "---- building Alacritty"
rustup override set stable
rustup update stable
cargo build --release
make app

echo ""
echo "---- add Alacritty.app to /Applications"
cp -r target/release/osx/Alacritty.app /Applications/


cd $START_DIR
