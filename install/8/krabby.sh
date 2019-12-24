#!/usr/bin/env bash

DEST_DIR=/usr/local/bin
TMP_DIR=/tmp
OLD_PWD=`pwd`

echo ""
echo "--- Krabby"
echo ""
echo "---- cloning..."

cd $TMP_DIR

git clone https://github.com/alexherbo2/krabby
cd krabby
make install static=yes extensions=yes interactive=no

cd $OLD_PWD
echo "fin."
