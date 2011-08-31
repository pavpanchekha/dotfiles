#!/bin/bash

export PATH="$PATH:$HOME/usr/bin"

# X server stuff
xbindkeys
setxkbmap -option caps:escape
xsetroot -solid '#000000' -cursor_name left_ptr

# Laptop Stuff
synclient JumpyCursorThreshold=100

# Applications
dropbox start
conky -d
xflux -z 94701

exec ssh-agent xmonad
