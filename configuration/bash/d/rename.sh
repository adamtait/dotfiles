#!/usr/bin/env bash

# rename
#   tool for re-naming many similar files
#   usage: rename *.ft s/\.ft/\.tt/g

rename() {
    if [[ ! $1 || ! $2 ]]; then
        echo -e "rename \nusage: rename <fileglob> <sed-script> \nexample: rename *.ft s/\.ft/\.tt/g"
        return 0
    fi
    
    while read f
    do
        mv "$f" $(echo "$f" | sed -e '$2')
    done <<< $(find . -name $1)
}    
