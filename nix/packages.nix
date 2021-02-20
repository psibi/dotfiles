with import ./nixpkgs.nix;
{
  inherit ripgrep
    # xscreensaver
    # alacritty;
    # google-chrome
    # slack
    sibi-xmonad rxvt_unicode-with-plugins feh screen xclip xsel xdotool eboard libqalculate
    gnucash fish emacs unrar pdftk kazam vlc ffmpeg bat exa fd procs nixfmt git direnv octave
    cabal2nix keepassxc jl rustup hlint htop powertop font-awesome symbola alegreya sibi-xmobar;
  hindent = haskellPackages.hindent;
  pandoc = haskellPackages.pandoc;
  latex = texlive.combined.scheme-full;
  pygments = python38Packages.pygments;
  sympy = python27Packages.sympy;
  # latex = texlive.combined.scheme-basic;
  sibi-utils = haskellPackages.callPackage ../utils/sibi-utils.nix {};
}
