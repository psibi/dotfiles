# ~/.nixpkgs/config.nix file
# Install like this: nix-env -iA nixpkgs.sibiHaskellEnv
# Probably need to reduce the global dependencies ?
{
  packageOverrides = super: let self = super.pkgs; in
  {
    sibiHaskellEnv = self.haskell.packages.ghc7101.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       xmonad-contrib
                       mtl
                       cabal-install
                       xmobar
                       http-client
                       http-conduit
                       conduit-combinators
                     ]);
  };
}
