#!/usr/bin/env bash

echo ""
echo "--- Python lang"


DEST_DIR=/tmp
FILENAME="python-3.9.10-macos11.pkg"
START_DIR=`pwd`

echo "---- downloading..."
curl -0L https://www.python.org/ftp/python/3.9.10/$FILENAME > $DEST_DIR/$FILENAME

cd $DEST_DIR

echo "---- installing Python 3.9.10 package (requires root)"
sudo installer -pkg  $FILENAME -target /
rm -rf $FILENAME

cd $START_DIR


