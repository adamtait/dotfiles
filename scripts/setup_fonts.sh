#!/bin/sh


if [[ ! -e $HOME/Library/Fonts/Anonymous\ Pro.ttf ]]; then
    echo "--- installing Anonymous Pro (emacs font)"
    cd /tmp
    curl http://www.marksimonson.com/assets/content/fonts/AnonymousPro-1.002.zip > /tmp/AnonymousPro-1.002.zip
    unzip /tmp/AnonymousPro-1.002.zip
    cp /tmp/AnonymousPro-1.002.001/*.ttf $HOME/Library/Fonts/
    rm -rf /tmp/*AnonymousPro*
fi
