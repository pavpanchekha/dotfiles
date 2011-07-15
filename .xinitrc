#!/bin/bash

export PATH="$PATH:$HOME/usr/bin"

# X server stuff
xbindkeys
xmodmap .keys
xsetroot -solid '#000000'

# Laptop Stuff
synclient JumpyCursorThreshold=100

# Applications
dropbox start
conky -d
xflux -z 94701
fetchmail -d60

bash $HOME/.config/dwm/bar.sh &disown
exec ssh-agent dwm
