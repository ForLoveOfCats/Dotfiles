#!/bin/bash


if pgrep -x "emacs" > /dev/null
then
    emacsclient -n $1
else
    emacs $1
fi 
