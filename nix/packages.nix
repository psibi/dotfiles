with import ./nixpkgs.nix;
{
  inherit ripgrep
    # xscreensaver
    # alacritty;
    sibi-xmonad rxvt_unicode-with-plugins feh screen xclip xsel xdotool eboard
    gnucash fish emacs unrar pdftk kazam vlc ffmpeg bat fd nixfmt git
    cabal2nix keepassxc google-chrome slack jl rustup hlint htop powertop font-awesome symbola sibi-xmobar;
  hindent = haskellPackages.hindent;
  pandoc = haskellPackages.pandoc;
  latex = texlive.combined.scheme-full;
  sibi-utils = haskellPackages.callPackage ../utils/sibi-utils.nix {};
}
