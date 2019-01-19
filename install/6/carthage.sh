#!/usr/bin/env bash

echo ""
echo "--- Carthage (Xcode dependency manager)"

TMP_PATH=/tmp
START_DIR=`pwd`
FILE_NAME=Carthage.pkg
VERSION=0.31.0


echo ""
echo "----> downloading $FILE_NAME - version $VERSION ..."
cd $TMP_PATH
curl -0L https://github.com/Carthage/Carthage/releases/download/$VERSION/$FILE_NAME > $FILE_NAME

echo ""
echo "----> installing $FILE_NAME - version $VERSION ..."
sudo installer -pkg $FILE_NAME -target /

rm -rf $FILE_NAME

cd $START_DIR



# FOR UNINSTALLING
# REPO_NAME=Carthage
# echo ""
echo "----> cloning Carthage/$REPO_NAME..."
# git clone git@github.com:Carthage/$REPO_NAME
# cd $REPO_NAME
# git checkout tags/0.31.0
# echo ""
echo "----> building Carthage/$REPO_NAME..."
# make install
# make uninstall
