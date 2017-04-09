Here you will find few of the configuration which I use in my Linux system. You can tailor it according it to your needs.

Files
------

* .alias - Useful alias file of commonly used UNIX commands.
* .global_ignore - Global git ignore rules.
* apt-fast - Script For fast downloading
* init.el - Sleek Emacs configuration file
* .bashrc - Some extra bash configuration
* .screenrc - GNU Screen Emacs Configuration file
* setup.sh - Devops script
* .Xresource - X resource settings
* emacsSync.hs - Emacs synchronization script


Softwares needed
-----------------

Haskell:

* xmonad
* xmonad-contrib
* xmonad-extras
* split
* xmobar

Note:

Make sure you install with `with_xft` and `with_iwlib` flag:

``` shellsession
cabal install -f with_xft -f with_iwlib xmobar
```

Distro related:

* xclip
* rxvt-unicode
* screen
* xsel
* xfce4-screenshooter
