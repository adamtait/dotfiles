#!/usr/bin/env bash


hash realpath 2>/dev/null || { echo >&2 "ERROR: This script requires \"realpath\" but it's not installed.  Please install it and try again. You might try 'brew link coreutils'."; exit 1; }


CURRENT_DIR=$PWD
DOTFILES_DIR=$CURRENT_DIR
cd $DOTFILES_DIR

echo "loading git submodules"
git submodule update --init --remote --checkout

if [[ ! -d $HOME ]]; then
    echo "please set $HOME before proceeding"
    exit 1
fi

# run all setup files
for f in $DOTFILES_DIR/setup/*
do
    sh $f;
done

cd $CURRENT_DIR
