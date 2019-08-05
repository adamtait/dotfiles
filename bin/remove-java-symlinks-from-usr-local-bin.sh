#!/usr/bin/env bash

echo ""
echo "--- Remove Java symlinks from /usr/local/bin"
echo ""

# -- check usage with 1st argument 
if [ $# == 0 ]
then
    echo "JDK bin source path required!"
    echo "usage: ./remove-java-symlinks-from-usr-local-bin.sh /usr/local/jdk-12.0.2.jdk/Contents/Home/bin/"
    echo ""
    echo ""
    exit 0
fi


filenames=$(ls $1)
for fname in ${filenames[@]}
do
    echo "rm /usr/local/bin/$fname"
    rm -f /usr/local/bin/$fname
done

echo "--- Fin. (Remove Java symlinks from /usr/local/bin)"
