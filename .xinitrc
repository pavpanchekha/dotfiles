#!/bin/bash

export PATH="$PATH:$HOME/bin"
xset +fp /usr/share/fonts/local

# X server stuff
[ `hostname` == "desktop" ] && xbindkeys
setxkbmap -option ctrl:swapcaps -option compose:lctrl
xsetroot -solid '#dddddd' -cursor_name left_ptr

# Applications
stalonetray -bg black &
dropbox start &
nm-applet &
feh --bg-fill ~/bg.jpg
redshift -l 37.8:-122.4 &
[ `hostname` == "desktop" ] && tracker-control --start &

# Time tracker
python ~/bin/timetracker -I 300 -b 60 -f ~/Dropbox/Data/timetrack.`hostname`.log &

eval $(gpg-agent --daemon)
exec ssh-agent xmonad
