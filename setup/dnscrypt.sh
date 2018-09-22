#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


echo -e "\n--- DNS Crypt setup"

D=$HOME/.dnscrypt

if [[ ! -d $D ]]; then
    echo "--- creating ${D}"
    mkdir $D
fi

# link config files
for file_path in $(find "$DOTFILES_DIR/configuration/dnscrypt/config/" -type f)
do
    file_name=$(basename "$file_path")
    if [[ ! -h $D/$file_name ]]; then
        echo "--- linking ${file_name} to ${D}"
        ln -s $file_path $D/
    fi
done


LD_FILE_PATH=/Library/LaunchDaemons/info.dnscrypt.proxy.plist
if [[ ! -h $LD_PATH ]]
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
    sudo ln -s $DOTFILES_DIR/dnscrypt/launchdaemon.plist $LD_FILE_PATH
fi
