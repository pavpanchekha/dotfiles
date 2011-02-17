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

<<<<<<< HEAD
xhost +local: >/dev/null
bash $HOME/.config/dwm/bar.sh &disown
exec ssh-agent dwm
=======
PATH="$PATH:$HOME/usr/bin" exec ssh-agent dwm
>>>>>>> 64050a0d213f2e6d4cd96ea7d2c71dab1aa4a29a
