with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "xmonad";
  buildInputs = [
    ghc
    zlib
    alsaLib
    pkg-config
    xorg.libX11
    xorg.libXrandr
    xorg.libXext
    xorg.libXScrnSaver
    xorg.libXft
    git
  ];
}
