#!/usr/bin/env bash

DEST_DIR=/usr/local/bin
TMP_DIR=/tmp
VERSION=0.12.21
FILENAME=terraform_${VERSION}_darwin_amd64.zip
OLD_PWD=`pwd`

echo ""
echo "--- TerraForm"
echo ""
echo "---- downloading..."

cd $TMP_DIR
curl -L0 https://releases.hashicorp.com/terraform/${VERSION}/$FILENAME > $FILENAME
unzip $FILENAME
rm -f $FILENAME

echo ""
echo "---- installing to ${DEST_DIR}"
mv terraform $DEST_DIR/

cd $OLD_PWD
echo "fin."
