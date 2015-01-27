# https://nixos.org/wiki/FAQ#How_can_I_manage_software_with_nix-env_like_with_configuration.nix.3F       
# Thanks to Lethalman from IRC for pointing me to the right direction
# Use nix-env -qaP ghc for finding out the proper variable name

with (import <nixpkgs> {});
 { inherit firefoxWrapper;
 }
 
# with (import <nixpkgs.haskellPackages> {});
# {
# inherit cabalInstall;
# }
 



