#!/usr/bin/env bash

ln -s $PWD/.xsession ~/.xsession

ln -s $PWD/.xmobarrc ~/.xmobarrc

## https://github.com/alacritty/alacritty#configuration
## You might also have to delete alacrity's original configuration
## Start with alacritty -v to find out if it's starting with any other configuration
ln -s $PWD/alacritty.yml ~/.alacritty.yml

# For Making sure fonts installed via nixpkgs is available
# Taken from https://github.com/NixOS/nixpkgs/issues/8318#issuecomment-356327657
ln -sf ~/.nix-profile/share/fonts/ ~/.local/share/fonts/nix-fonts

./emacs.sh

