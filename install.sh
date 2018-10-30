#!/usr/bin/env bash
#
# INSTALL
# re-install (or install fresh) all the apps you like (where you = Adam)
# code & ideas are largely taken from http://lapwinglabs.com/blog/hacker-guide-to-setting-up-your-mac

echo -e "\n--- install.sh --- Begin."

for f in $DOTFILES_DIR/install/**/*
do
    sh $f
done

echo -e "\n--- install.sh --- Complete."
