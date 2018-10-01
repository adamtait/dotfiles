#!/usr/bin/env bash

echo -e "\n--- DNS Crypt setup"


DOTFILES_DIR=$(dirname $(realpath "$0"))/..
CONFIG_DIR=$DOTFILES_DIR/configuration/dnscrypt/config
D=$HOME/.dnscrypt

if [[ ! -d $D ]]; then
    echo "--- creating ${D}"
    mkdir $D
fi


# create local blacklist
./dnscrypt.blacklist.sh "$D/domains.blacklist.txt"


# link config files
for file_path in $(find "$CONFIG_DIR/" -type f)
do
    file_name=$(basename "$file_path")
    if [ ! -e $D/$file_name ]; then
        echo "--- linking ${file_name} to ${D}"
        ln -s $file_path $D/
    fi
done


LD_FILE_PATH=/Library/LaunchDaemons/dnscrypt-proxy.plist
if [[ ! -e $LD_FILE_PATH ]]
then
    # setup Launch Daemon

    PLIST_DIR=$DOTFILES_DIR/configuration/dnscrypt
    TEMPLATE_FILE_PATH=$PLIST_DIR/launchdaemon.plist.template
    SRC_FILE_PATH=$PLIST_DIR/launchdaemon.plist

    if [[ -f $SRC_FILE_PATH ]]; then
        echo -e "\n---- removing old system LaunchDaemon for DNS Crypt"
        sudo rm -f $SRC_FILE_PATH
    fi
        
    sed \
        -e "s#{{COMMAND}}#/usr/local/bin/dnscrypt-proxy#g" \
        -e "s#{{WORKING_DIR}}#$D#g" \
        $TEMPLATE_FILE_PATH > $SRC_FILE_PATH

    echo -e "\n---- linking system LaunchDaemon for DNS Crypt"
    chmod 544 $SRC_FILE_PATH
    sudo chown root:wheel $SRC_FILE_PATH
    sudo ln -s $SRC_FILE_PATH $LD_FILE_PATH
fi
