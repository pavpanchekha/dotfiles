#!/bin/bash

export PATH="$PATH:$HOME/usr/bin"
xset +fp /usr/share/fonts/local

# X server stuff
xbindkeys
setxkbmap -option caps:escape
xsetroot -solid '#dddddd' -cursor_name left_ptr

# Applications
dropbox start &
conky -d
xflux -z 02142
/usr/lib/notification-daemon-1.0/notification-daemon 2>/dev/null &
tracker-control --start &

conky -c .config/bar.conky | dzen2 -w 600 -x 766 -fg white -bg black -fn -*-terminus-medium-*-*-*-*-140-*-*-*-*-*-* -ta r &
exec ssh-agent xmonad
