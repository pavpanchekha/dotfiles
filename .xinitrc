#!/bin/zsh

xmobar ~/.xmonad/xmobar.hs &
stalonetray -c ~/.xmonad/stalonetrayrc &
gnome-settings-daemon
gnome-keyring-daemon
fetchmail -d60
empathy &
nm-applet &
dropbox start
.conky/run
xhost +local: >/dev/null
exec ssh-agent xmonad
