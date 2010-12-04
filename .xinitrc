#!/bin/zsh

xmobar ~/.xmonad/xmobar.hs &
stalonetray -c ~/.xmonad/stalonetrayrc &
xbindkeys
gnome-settings-daemon
gnome-keyring-daemon
fetchmail -d60
empathy &
dropbox start
.conky/run
xhost +local: >/dev/null
exec ssh-agent xmonad
