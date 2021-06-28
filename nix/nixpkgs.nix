# https://github.com/NixOS/nixpkgs/tags

import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz") {
    config = import ./configuration.nix;
  }
