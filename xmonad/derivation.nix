let
  pkgs = import <nixpkgs> {};
in
{
  sibi-xmonad = pkgs.haskellPackages.callPackage ./default.nix {};
}
