#!/usr/bin/env bash

DEST_DIR=/usr/local/bin
TMP_DIR=/tmp
VERSION=2.1.1
FILENAME=bazel-${VERSION}-installer-darwin-x86_64.sh
OLD_PWD=`pwd`


echo ""
echo "--- Bazel"
echo ""
echo "---- downloading..."

cd $TMP_DIR
curl -L0 https://github.com/bazelbuild/bazel/releases/download/${VERSION}/$FILENAME > $FILENAME
chmod +x $FILENAME
./$FILENAME

rm -f $FILENAME

cd $OLD_PWD
echo "--- fin ---"
