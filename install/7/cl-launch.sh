#!/usr/bin/env bash

DEST_DIR=/tmp
TARGET_NAME="cl-launch-4.1.4"
TAR_DIR=$TARGET_NAME
FILENAME="$TARGET_NAME.tar.gz"
START_DIR=`pwd`


echo ""
echo "--- cl-launch (Common LISP runtime)"
echo "---- downloading..."
curl -0L https://common-lisp.net/project/xcvb/cl-launch/$FILENAME > $DEST_DIR/$FILENAME

cd $DEST_DIR
tar -zxf $FILENAME



cd $TAR_DIR

echo "----- fixing Makefile"
sed "s/@cp/cp/g" Makefile > Makefile.tmp && mv Makefile.tmp Makefile

echo "---- installing cl-launch to /usr/local (requires sudo)"
echo "[ $TARGET_NAME; sudo make install ]"
sudo make install

# return to starting directory; remove /tmp files
cd $START_DIR
rm -rf $DEST_DIR/$TAR_DIR $DEST_DIR/$FILENAME
