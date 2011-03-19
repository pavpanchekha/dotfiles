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
PATH="$PATH:$HOME/usr/bin" .conky/run
xflux -z 02142
emacs --daemon

bash $HOME/.config/dwm/bar.sh &disown
PATH="$PATH:$HOME/usr/bin" exec ssh-agent dwm
