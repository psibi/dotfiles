with import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz") { };
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
  ];
}
