#!/bin/bash

export PATH="$PATH:$HOME/bin"
xset +fp /usr/share/fonts/local

# X server stuff
xbindkeys
setxkbmap -option caps:escape
xsetroot -solid '#dddddd' -cursor_name left_ptr

# Applications
dropbox start &
conky -d
redshift -l 42.359947:-71.08846 &
tracker-control --start &

# Time tracker
python ~/dev/TimeTracker/src/x11.py >>~/data/timetrack.`hostname`.log &

exec ssh-agent xmonad
