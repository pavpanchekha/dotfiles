#!/bin/bash
set -e

export PATH="$PATH:$HOME/bin"
export GTK_IM_MODULE=xim
[ -d /usr/share/fonts/local ] && xset +fp /usr/share/fonts/local

# X server stuff
xkbcomp ~/.config/xkeyboard $DISPLAY
xsetroot -cursor_name arrow

# Applications
stalonetray -c .config/stalonetray &
nm-applet &

feh --bg-fill ~/Dropbox/bg-16x9.png &

redshift -mrandr -l 37.8:-122.4 &
which start-pulseaudio-x11 && start-pulseaudio-x11

# Time tracker
python3 ~/bin/timetracker -I 300 -b 60 -f ~/Dropbox/Data/timetrack.`hostname`.log &

eval $(gpg-agent --daemon)

# Start system stuff
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
        for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

dropbox start &

exec ssh-agent xmonad
