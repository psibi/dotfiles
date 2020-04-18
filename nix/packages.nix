with import ./nixpkgs.nix;
{
  inherit ripgrep
    # xscreensaver
    sibi-xmonad rxvt_unicode-with-plugins feh screen xclip xsel xdotool eboard
    xmobar gnucash fish emacs unrar pdftk kazam vlc ffmpeg bat fd nixfmt git
    cabal2nix keepassxc google-chrome slack jl rustup hlint htop;
  hindent = haskellPackages.hindent;
  latex = texlive.combined.scheme-full;
}
