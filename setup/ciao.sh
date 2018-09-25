#!/usr/bin/env bash

echo -e "\n--- CIAO setup"

DOTFILES_DIR=$(dirname $(realpath "$0"))/..
CIAO_SCRIPT_DIR=$DOTFILES_DIR/submodules/ciao
CONFIG_DIR=$DOTFILES_DIR/configuration/ciao
CIAO_HOME_DIR=~/.ciao


# run ciao setup
sh $CIAO_SCRIPT_DIR/setup.sh


# symlink .ciao/config
if [[ -f $CIAO_HOME_DIR/config ]]; then
    rm $CIAO_HOME_DIR/config
fi


echo -e "\n--- symlink ~/.ciao/config"
ln -s $CONFIG_DIR/config $CIAO_HOME_DIR/config
