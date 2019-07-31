#!/usr/bin/env bash


echo ""
echo "--- Amazon Web Service CLI"


DEST_DIR=/tmp
FILENAME="awscli-bundle.zip"
START_DIR=`pwd`


echo "---- downloading..."
curl "https://s3.amazonaws.com/aws-cli/$FILENAME" -o "$DEST_DIR/$FILENAME"

cd $DEST_DIR

echo "---- running installer. installing to /usr/local/aws with symlinks in /usr/local/bin/aws"
unzip $FILENAME
sudo ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws

rm -rf $FILENAME awscli-bundle

cd $START_DIR
