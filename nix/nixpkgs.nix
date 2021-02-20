# https://github.com/NixOS/nixpkgs/tags

import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {
    config = import ./configuration.nix;
  }
