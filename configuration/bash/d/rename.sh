#!/usr/bin/env bash

# rename
#   tool for re-naming many similar files
#   usage: rename *.ft s/\.ft/\.tt/g

rename() {
    if [[ ! $1 || ! $2 ]]; then
        echo ""
        echo "rename"
        echo "usage: rename <fileglob> <sed-script>"
        echo "example: rename *.ft s/\.ft/\.tt/g"
        return 0
    fi
    
    while read f
    do
        mv "$f" $(echo "$f" | sed -e '$2')
    done <<< $(find . -name $1)
}    
