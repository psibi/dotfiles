import (fetchTarball
  "https://releases.nixos.org/nixos/19.09/nixos-19.09.2079.8731aaaf8b3/nixexprs.tar.xz") {
    config = import ./configuration.nix;
  }
