#!/usr/bin/env bash

echo ""
echo "--- Remove Java symlinks from /usr/local/bin"
echo ""

filenames=$(ls /usr/local/jdk-12.0.2.jdk/Contents/Home/bin/)
for fname in ${filenames[@]}
do
    echo "rm /usr/local/bin/$fname"
    rm -f /usr/local/bin/$fname
done

echo "--- Fin. (Remove Java symlinks from /usr/local/bin)"
