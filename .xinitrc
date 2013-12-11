#!/bin/bash

export PATH="$PATH:$HOME/bin"
xset +fp /usr/share/fonts/local

# X server stuff
[ `hostname` == "desktop" ] && xbindkeys
setxkbmap -option caps:escape
xsetroot -solid '#dddddd' -cursor_name left_ptr

# Applications
dropbox start &
conky -c ~/.config/conky/bottom_left.cfg -d
conky -c ~/.config/conky/bottom_right.cfg -d
feh --bg-fill ~/wallpaper.jpg
redshift -l 37.8:-122.4 &
[ `hostname` == "desktop" ] && tracker-control --start &

# Time tracker
python ~/bin/timetracker -b 60 -f ~/Dropbox/Data/timetrack.`hostname`.log &

eval $(gpg-agent --daemon)
exec ssh-agent xmonad
