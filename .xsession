#!/bin/bash
 
# Load resources
 
xrdb -merge .Xresources

xmodmap ~/.Xmodmap
# Set up an icon tray
 
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x00ff00 --height 12 &
 
# Set the background color<
 
xsetroot -solid midnightblue
 
# Fire up apps
 
xscreensaver -no-splash &
 
if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

# Because I like GNOME's settings and themes
if [ -x /usr/bin/gnome-settings-daemon ] ; then
   sleep 3
   gnome-settings-daemon &
fi

if [ -x /usr/bin/gnome-sound-applet ] ; then
    sleep 3
    gnome-sound-applet &
fi    

# Doesn't ask password everytime
eval `ssh-agent`
xterm -e ssh-add

exec xmonad
