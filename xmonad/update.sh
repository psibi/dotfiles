#!/usr/bin/env bash

stack install --local-bin-path /home/sibi/.xmonad/
cp xmonad.hs ~/.xmonad/
cd ~/.xmonad
echo "Files inside ~/.xmonad: "
ls xmona*
rm -v xmonad-x86_64-linux
cp -v xmonad xmonad-x86_64-linux
