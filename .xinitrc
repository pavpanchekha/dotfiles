#!/bin/bash

xmobar ~/.xmonad/xmobar.hs &
xbindkeys
xmodmap .keys

fetchmail -d60
dropbox start
gajim &disown
PATH="$PATH:$HOME/usr/bin" .conky/run

xhost +local: >/dev/null

exec ssh-agent xmonad
