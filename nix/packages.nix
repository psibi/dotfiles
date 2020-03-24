with import <nixpkgs> { config = import ./configuration.nix; }; {
  inherit 
  ripgrep 
  # xscreensaver
  rxvt_unicode-with-plugins
  feh
  screen
  xclip
  xsel
  xdotool
  eboard
  gnucash
  fish
  emacs
  unrar
  pdftk
  git;
}
