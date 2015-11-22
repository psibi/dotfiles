#!/bin/bash
 
# Load resources
 
xrdb -merge ~/.Xresources

# Set the background color<

# dmenu was throwing Segmentation Error
export LANG="en_US.utf8" 
 
# Fire up apps
 
xscreensaver -no-splash &
 
# Because I like GNOME's settings and themes
if [ -x /usr/bin/gnome-settings-daemon ] ; then
   sleep 3
   gnome-settings-daemon &
fi

# if [ -x /usr/bin/gnome-sound-applet ] ; then
#     sleep 3
#     gnome-sound-applet &
# fi    

xmodmap ~/.Xmodmap
# xmodmap -e 'keycode 135=Super_L' #Only for my laptop

#Audio failed working
# Some try
# 1) Add username to the group audio
# 2) sudo alsamixer (Make it unmute)
# Restarting makes it work

# Note that inovking these apps beforehand caused xmobar to disappear.
# Probably some window arrangment was being done ?

# gnome-terminal -e screen &
# firefox &
# emacs &
# nautilus &
# ssh-add &

# Choose your Window Manager
# exec gnome-session
# exec startkde
# exec startxfce4
# exec xmonad

# In Debian based systems, manage your network connections using nm-connection-editor

exec xmonad
