#!/bin/bash

xbindkeys
xmodmap .keys
xsetroot -solid '#111111'

nm-applet &
synclient JumpyCursorThreshold=100

dropbox start
gajim &disown
PATH="$PATH:$HOME/usr/bin" .conky/run

xhost +local: >/dev/null
bash $HOME/.config/dwm/bar.sh &disown
exec ssh-agent dwm
