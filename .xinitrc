#!/bin/bash

. .bash_profile

xmobar ~/.xmonad/xmobar.hs &
stalonetray -c ~/.xmonad/stalonetrayrc &
xbindkeys
gnome-settings-daemon
gnome-keyring-daemon
seahorse-agent
empathy &
dropbox start
.conky/run
xhost +local: >/dev/null
exec xmonad
