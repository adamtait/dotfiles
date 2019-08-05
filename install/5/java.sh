#!/usr/bin/env bash

echo ""
echo "--- Java (OpenJDK)"


TEMP_DIR=/tmp/java
FILENAME="openjdk-11+28_linux-x64_bin.tar.gz"
START_DIR=`pwd`

echo "---- downloading..."

mkdir $TEMP_DIR
curl -0L https://download.java.net/openjdk/jdk11/ri/$FILENAME > $TEMP_DIR/$FILENAME

echo "---- unzipping..."
cd $TEMP_DIR
tar zxvf $FILENAME

echo "---- relocating to /usr/local (requires sudo)"
JDK_DIR_NAME="jdk-11"
sudo mv $JDK_DIR_NAME /usr/local/

echo "---- symlinking to /usr/local/bin"
ln -s /usr/local/$JDK_DIR_NAME/bin/* /usr/local/bin/



# clean up
cd $START_DIR
rm -rf $TEMP_DIR


echo "--- Installed. (Java OpenJDK)"
