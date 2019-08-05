#!/usr/bin/env bash

echo ""
echo "--- Java (OpenJDK)"


TEMP_DIR=/tmp
FILENAME="openjdk-12.0.2_osx-x64_bin.tar.gz"
START_DIR=`pwd`

echo "---- downloading..."
curl -0L https://download.java.net/java/GA/jdk12.0.2/e482c34c86bd4bf8b56c0b35558996b9/10/GPL/$FILENAME > $TEMP_DIR/$FILENAME

echo "---- unzipping..."
cd $TEMP_DIR
tar zxvf $FILENAME

echo "---- relocating to /usr/local (requires sudo)"
JDK_DIR_NAME="jdk-12.0.2.jdk"
sudo mv $JDK_DIR_NAME /usr/local/

echo "---- symlinking to /usr/local/bin"
ln -s /usr/local/$JDK_DIR_NAME/Contents/Home/bin/* /usr/local/bin/



# clean up
rm -rf $FILENAME

cd $START_DIR


echo "--- Installed. (Java OpenJDK)"
