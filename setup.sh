#!/bin/zsh

# load the git submodules
#git submodule init   # may not be necessary
git submodule update

ln -s zshrc.symlink ~/.zshrc
ln -s emacs.symlink ~/.emacs
ln -s slate.symlink ~/.slate