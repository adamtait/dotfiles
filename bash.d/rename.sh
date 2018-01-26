#!/usr/bin/env bash

# rename
#   tool for re-naming many similar files
#   usage: rename *.ft s/\.ft/\.tt/g

rename() {
    while read f
    do
        mv "$f" $(echo "$f" | sed -e '$2')
    done <<< $(find . -name $1)
}    
