#!/bin/bash

export PATH="$PATH:$HOME/bin"
xset +fp /usr/share/fonts/local

# X server stuff
[ `hostname` == "desktop" ] && xbindkeys
setxkbmap -option caps:escape
xsetroot -solid '#dddddd' -cursor_name left_ptr

# Applications
dropbox start &
conky -d
redshift -l 37.8:-122.4 &
[ `hostname` == "desktop" ] && tracker-control --start &

# Time tracker
python ~/dev/TimeTracker/src/timetracker.py -b 60 -f ~/data/timetrack.`hostname`.log &

exec ssh-agent xmonad
