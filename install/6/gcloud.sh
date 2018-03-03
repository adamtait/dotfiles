#!/usr/bin/env bash


if [[ ! -d $HOME/workspace ]]; then
    echo -e "\n--- creating ~/workspace directory"
    mkdir $HOME/workspace
fi


DEST_DIR=$HOME/workspace
FILENAME="google-cloud-sdk-191.0.0-darwin-x86_64.tar.gz"
START_DIR=`pwd`


echo -e "\n--- gcloud (Google Cloud cmd-line SDK)"
echo -e "\n---- downloading..."
curl -0L https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/$FILENAME > $DEST_DIR/$FILENAME
cd $DEST_DIR
tar -zxf $FILENAME

echo -e "\n---- running installer"
echo -e "---- !!IMPORTANT!!     DO NOT _Modify profile to update your $PATH and enable shell command completion?_"
echo -e "----    When the installer asks to modify your $PATH, just say NO. We've already done that!"
./google-cloud-sdk/install.sh

cd $START_DIR
