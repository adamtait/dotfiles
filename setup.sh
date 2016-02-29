#!/bin/zsh

CURRENT_DIR=$PWD
DOTFILES_DIR=$0:a:h
cd $DOTFILES_DIR

echo "loading git submodules"
git submodule init
git submodule update

if [[ ! -d $HOME ]]; then
    echo "please set $HOME before proceeding"
    exit 1
fi

echo "-- EMACS -----------------------"
sh $DOTFILES_DIR/scripts/setup_emacs.sh

echo "-- $PATH -----------------------"
sh $DOTFILES_DIR/scripts/setup_paths.sh

echo "-- SYMLINKS --------------------"
sh $DOTFILES_DIR/scripts/setup_symlinks.sh

echo "-- GITCONFIG -------------------"
sh $DOTFILES_DIR/scripts/setup_gitconfig.sh

echo "-- OSX SETTINGS ----------------"
SUDO defaults write com.apple.Finder AppleShowAllFiles YES; killall Finder

echo "-- FONTS -----------------------"
sh $DOTFILES_DIR/scripts/setup_fonts.sh

echo "-- BAZEL (GOOGLE BUILD TOOL) ---"
sh $DOTFILES_DIR/scripts/setup_bazel.sh

cd $CURRENT_DIR
