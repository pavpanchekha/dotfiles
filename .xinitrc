#!/bin/bash

export PATH="$PATH:$HOME/usr/bin"

# X server stuff
xbindkeys
xmodmap .keys
xsetroot -solid '#000000'
xset +fp /usr/share/fonts/local/
xhost +local: >/dev/null

# Applications
fetchmail -d60
dropbox start
conky -d
xflux -z 02142

bash $HOME/.config/dwm/bar.sh &disown
exec ssh-agent dwm
