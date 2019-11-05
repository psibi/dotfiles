# Fresh System

* fish
* git
* gcc
* stack
* emacs
* xscreensaver
* feh
* slack
* keybase
* screen
* rxvt-unicode
* xfce4-screenshooter
* xclip
* xsel
* xdotool
* [emacs-everywhere](https://github.com/psibi/emacs-everywhere)
* eboard
* google-chrome
* zoom
* gnucash
* [keepassxc](https://github.com/keepassxreboot/keepassxc)
* unrar
* tldr
* pavucontrol
* libasound2-dev
* libx11-dev
* libxrandr-dev
* libxss-dev
* libtinfo-dev
* libxft-dev
* libiw-dev
* rust
* ripgrep
* hlint
* hindent

## Debian/Ubuntu quirks

* gnome-tweaks

## Haskell softwares

* xmonad
* xmonad-contrib
* xmonad-extras
* xmobar

Make sure you install `xmobar` with `with_xft` and `with_iwlib` flag:

``` shellsession
stack install --flag xmobar:with_xft --flag xmobar:with_iwlib
```

## Audio System

On xmonad, invoke gnome-control-center like this:

``` shellsession
env XDG_CURRENT_DESKTOP=GNOME gnome-control-center
```

And then you can adjust your microphone accordingly.

Testing tool:
* [onlinemic test](https://www.onlinemictest.com)
* [webcam test](https://www.onlinemictest.com/webcam-test/)
* [sound test](https://www.onlinemictest.com/sound-test/)

Working configuration in Laptop (elric):
* Profile: Analog Stereo Duplex



## Other links

* [Non graphical boot with systemd](https://unix.stackexchange.com/a/164028/29539)
* [Boot into shell](https://askubuntu.com/questions/148717/how-do-i-boot-into-the-console-and-then-launch-the-ubuntu-desktop-from-it?noredirect=1&lq=1)
* [Bluetooth issue](https://askubuntu.com/a/1009114)
* [gnome-control-center is empty](https://www.reddit.com/r/archlinux/comments/75zpfe/gnomecontrolcenter32611_is_empty/)
