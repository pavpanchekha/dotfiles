#!/bin/bash

xbindkeys
xmodmap .keys
xsetroot -solid black

stalonetray -c ~/.xmonad/stalonetrayrc &
nm-applet &
synclient JumpyCursorThreshold=100

dropbox start
gajim &disown
PATH="$PATH:$HOME/usr/bin" .conky/run

xhost +local: >/dev/null

exec ssh-agent xmonad
