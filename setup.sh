#!/usr/bin/env bash

CURRENT_DIR=$PWD
DOTFILES_DIR=$CURRENT_DIR
cd $DOTFILES_DIR

echo "loading git submodules"
git submodule init
git submodule update

if [[ ! -d $HOME ]]; then
    echo "please set $HOME before proceeding"
    exit 1
fi

# run all setup files
for f in $DOTFILES_DIR/setup/*; do
    sh $f;
done

cd $CURRENT_DIR
