# Fresh System

* gcc
* stack
* keybase
* [emacs-everywhere](https://github.com/psibi/emacs-everywhere)
* zoom 
* tldr
* pavucontrol

## Handled via nix

* libasound2-dev
* [keepassxc](https://github.com/keepassxreboot/keepassxc)
* google-chrome
* libx11-dev
* libxrandr-dev (xorg.libXrandr)
* libxss-dev
* libtinfo-dev
* libxft-dev (xorg.libxft)
* libiw-dev (wirelesstools)
* rust
* hlint
* hindent
* git
* ripgrep
* xscreensaver
* feh
* screen
* rxvt-unicode
* xfce4-screenshooter
* xclip
* emacs
* xsel
* xdotool
* eboard
* gnucash
* unrar
* fish
* slack

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
* pavucontrol Profile: Analog Stereo Duplex
* Google hangouts setting: default (for both mic and speaker)

### Desktop machine

pavucontrol settings

* Input Device: Internal Microphone
* 14% optimal

## Other links

* [Non graphical boot with systemd](https://unix.stackexchange.com/a/164028/29539)
* [Boot into shell](https://askubuntu.com/questions/148717/how-do-i-boot-into-the-console-and-then-launch-the-ubuntu-desktop-from-it?noredirect=1&lq=1)
* [Bluetooth issue](https://askubuntu.com/a/1009114)
* [gnome-control-center is empty](https://www.reddit.com/r/archlinux/comments/75zpfe/gnomecontrolcenter32611_is_empty/)
* [Make default app](https://stackoverflow.com/a/50893809)

## Wireless

* [Making the router as repeater](http://electrodisc.com/dd-wrt/atheros/repeater.htm)
* [Check Acess point types](https://unix.stackexchange.com/a/62271/29539)
* [Real life speed of wireless networks](https://www.speedguide.net/faq/what-is-the-actual-real-life-speed-of-wireless-374)
* [Interpreting Signal strength](https://www.speedguide.net/faq/what-is-the-actual-real-life-speed-of-wireless-374)
* [Get station information](https://superuser.com/a/1146324/177053)
