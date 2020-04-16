with import ./nixpkgs.nix; {
  inherit ripgrep
    # xscreensaver
    sibi-xmonad rxvt_unicode-with-plugins feh screen xclip xsel xdotool eboard
    xmobar gnucash fish emacs unrar pdftk kazam vlc ffmpeg bat fd nixfmt git
    cabal2nix keepassxc;
}
