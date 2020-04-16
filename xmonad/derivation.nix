let
  pkgs = import ../nix/nixpkgs.nix;
in {
  sibi-xmonad = pkgs.haskellPackages.callPackage ./default.nix {};
}
