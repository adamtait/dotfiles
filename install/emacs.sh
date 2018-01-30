#!/bin/sh


# check for existing emacs install
if [[ ! `emacs -version` =~ "25" ]]; then
    echo "removing old emacs version"
    sudo rm /usr/bin/emacs
    sudo rm -rf /usr/share/emacs
fi
