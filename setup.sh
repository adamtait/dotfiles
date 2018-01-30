#!/bin/sh

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

echo "-- OSX SETTINGS ----------------"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder

echo "-- PATHs -----------------------"
sh $DOTFILES_DIR/setup/setup_paths.sh

echo "-- SYMLINKs --------------------"
sh $DOTFILES_DIR/setup/setup_symlinks.sh

echo "-- FONT(s) ---------------------"
sh $DOTFILES_DIR/setup/setup_fonts.sh

echo "-- EMACS -----------------------"
sh $DOTFILES_DIR/setup/setup_emacs.sh

echo "-- GITCONFIG -------------------"
sh $DOTFILES_DIR/setup/setup_gitconfig.sh

echo "-- BAZEL (GOOGLE build tool) ---"
sh $DOTFILES_DIR/setup/setup_bazel.sh

cd $CURRENT_DIR
