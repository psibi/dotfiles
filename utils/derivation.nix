let
  pkgs = import ../nix/nixpkgs.nix;
  utilPackage = pkgs.haskellPackages.callPackage ./default.nix {};
in utilPackage

