#!/usr/bin/env bash


echo ""
echo "--- Amazon Web Service CLI"


DEST_DIR=/tmp
FILENAME="AWSCLIV2.pkg"


echo "---- downloading..."
curl "https://awscli.amazonaws.com/$FILENAME" -o "$DEST_DIR/$FILENAME"


echo "---- running installer"
sudo installer -pkg $DEST_DIR/$FILENAME -target /
rm -rf $DEST_DIR/$FILENAME
