#!/bin/bash
#
# INSTALL
# re-install (or install fresh) all the apps you like (where you = Adam)
# code & ideas are largely taken from http://lapwinglabs.com/blog/hacker-guide-to-setting-up-your-mac

sh $DOTFILES_DIR/install/homebrew.sh
sh $DOTFILES_DIR/install/bash4.sh
sh $DOTFILES_DIR/install/gnu_utils.sh
sh $DOTFILES_DIR/install/osx_utils.sh

export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH

sh $DOTFILES_DIR/install/cmd_line_utils.sh
sh $DOTFILES_DIR/install/emacs.sh
sh $DOTFILES_DIR/install/osx_apps.sh
sh $DOTFILES_DIR/install/python.sh
sh $DOTFILES_DIR/install/ruby.sh
sh $DOTFILES_DIR/install/cocoapods.sh
sh $DOTFILES_DIR/install/dart.sh
sh $DOTFILES_DIR/install/bazel.sh

echo -e "\n--- install.sh --- Complete."
