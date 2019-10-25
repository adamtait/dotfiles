#!/usr/bin/env bash

DEST_DIR=/usr/local/bin
TMP_DIR=/tmp
FILENAME=terraform_0.12.12_darwin_amd64.zip
OLD_PWD=`pwd`

echo ""
echo "--- TerraForm"
echo ""
echo "---- downloading..."

cd $TMP_DIR
curl -L0 https://releases.hashicorp.com/terraform/0.12.12/$FILENAME > $FILENAME
unzip $FILENAME
rm -f $FILENAME

echo ""
echo "---- installing to ${DEST_DIR}"
mv terraform $DEST_DIR/

cd $OLD_PWD
echo "fin."
