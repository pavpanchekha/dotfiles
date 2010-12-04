#!/bin/bash

xbindkeys
xmodmap .keys

stalonetray -c ~/.xmonad/stalonetrayrc &
fetchmail -d60
nm-applet &

dropbox start
gajim &disown
PATH="$PATH:$HOME/usr/bin" .conky/run

xhost +local: >/dev/null

exec ssh-agent xmonad
