# ~/.nixpkgs/config.nix file
# Install like this: nix-env -f "<nixpkgs>" -iA sibiHaskellEnv             
{
  packageOverrides = super: let self = super.pkgs; in
  {
    sibiHaskellEnv = self.haskell.packages.ghc7101.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       xmonad-contrib
                       mtl
                       cabal-install
                       xmobar
                     ]);
  };
}
