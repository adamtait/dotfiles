#!/usr/bin/env bash

echo ""
echo "--- Clojure dev config setup"

DOTFILES_PATH=$(dirname $(realpath "$0"))/..
CONFIG_PATH=$DOTFILES_PATH/configuration/clojure
HOME_PATH=~/.clojure


# create ~/.clojure
if [[ ! -d $HOME_PATH ]]; then
    mkdir $HOME_PATH
fi

# symlink files
for config_file_path in $CONFIG_PATH/*
do
    f=$(basename $config_file_path)
    if [ -f $HOME_PATH/$f ] || [ -h $HOME_PATH/$f ]
    then
        echo "--- rm $HOME_PATH/$f"
        rm $HOME_PATH/$f
    fi

    echo "--- symlink $HOME_PATH/$f"
    ln -s $CONFIG_PATH/$f $HOME_PATH/$f
done


echo "-- fin --"
