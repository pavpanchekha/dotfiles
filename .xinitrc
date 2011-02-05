#!/bin/bash

# X server stuff
xbindkeys
xmodmap .keys
xsetroot -solid '#111111'
xset +fp /usr/share/fonts/local/
xhost +local: >/dev/null

# Applications
fetchmail -d60
dropbox start
gajim &disown
PATH="$PATH:$HOME/usr/bin" .conky/run

exec ssh-agent dwm
