#!/bin/zsh

# load the git submodules
echo "loading git submodules"
git submodule init
git submodule update

echo "setting up symlinks"
ln -s $PWD/zshrc.symlink ~/.zshrc
ln -s $PWD/emacs.symlink ~/.emacs
ln -s $PWD/slate.symlink ~/.slate