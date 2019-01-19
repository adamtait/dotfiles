#!/usr/bin/env bash

DEST_DIR=/tmp
FILENAME="dnscrypt-proxy-macos-2.0.16.tar.gz"
START_DIR=`pwd`


echo ""
echo "--- DNS Crypt (secure DNS proxy)"
echo ""
echo "---- downloading..."
curl -0L https://github.com/jedisct1/dnscrypt-proxy/releases/download/2.0.16/$FILENAME > $DEST_DIR/$FILENAME

cd $DEST_DIR
tar -zxf $FILENAME
TAR_DIR=macos     # directory that dnscrypt tarball extracts to. subject to change

echo ""
echo "---- installing dnscrypt-proxy to /usr/local/bin"
mv $TAR_DIR/dnscrypt-proxy /usr/local/bin/dnscrypt-proxy

rm -rf $TAR_DIR $FILENAME

echo ""
echo "---- [IMPORTANT] update your MacOS network connection DNS entries to include only '127.0.0.1' to use DNS Crypt."
echo "----- for more information, visit: https://github.com/jedisct1/dnscrypt-proxy/wiki/Installation-macOS"

cd $START_DIR

