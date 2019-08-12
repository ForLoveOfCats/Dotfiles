#!/bin/bash

#When Emacs starts my init file starts the server mode
#so the idea is that this script is launched *as* my
#editor by the system when I double click a text file
#and it can intelligently either start Emacs or connect
#to the running instance


if pgrep -x "emacs" > /dev/null
then
    emacsclient -n "$1"
else
    emacs "$1"
fi
