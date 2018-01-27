#! /bin/sh

# start emacs server
/Applications/Emacs.app/Contents/MacOS/Emacs --daemon

# start emacs client
/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t
