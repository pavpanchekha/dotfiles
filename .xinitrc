#!/bin/bash
set -e -x

export PATH="$PATH:$HOME/bin"
export GTK_IM_MODULE=xim
[ -d /usr/share/fonts/local ] && xset +fp /usr/share/fonts/local

# X server stuff
[ `hostname` == "desktop" ] && xbindkeys
setxkbmap -option ctrl:swapcaps -option compose:lctrl
xsetroot -cursor_name arrow
eval $(cat ~/.fehbg)

# Applications
stalonetray -c .config/stalonetray &
nm-applet &

sh ~/.fehbg &
redshift -l 37.8:-122.4 &
[ `hostname` == "desktop" ] && tracker-control --start &
which start-pulseaudio-x11 && start-pulseaudio-x11

# Time tracker
python3 ~/bin/timetracker -I 300 -b 60 -f ~/Dropbox/Data/timetrack.`hostname`.log &

eval $(gpg-agent --daemon)

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

exec ssh-agent xmonad
