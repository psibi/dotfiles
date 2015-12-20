# ~/.nixpkgs/config.nix file
# Install like this: nix-env -iA nixpkgs.sibiHaskellEnv
# Build nix-build "<nixpkgs>" -A haskellPackages.xmonad-contrib
# Sample priority command: nix-env --set-flag priority 1 xmonad
{
  allowUnfree = true; 
  
  packageOverrides = super: 
  let self = super.pkgs; 
      xmonad = self.callPackage /home/sibi/github/xmonad/default.nix {};
      xmonad-contrib = self.callPackage /home/sibi/github/xmonad-contrib/default.nix {};
  in
  {

  haskellPackages = super.haskellPackages.override {
          overrides = self: super: rec {
                    sibi-postgresql = self.callPackage /home/sibi/github/persistent/persistent-postgresql/default.nix {};
                    sibi-sqlite = self.callPackage /home/sibi/github/persistent/persistent-sqite/default.nix {};
                    sibi-mysql = self.callPackage /home/sibi/github/persistent/persistent-mysql/default.nix {};
                    sibi-persistent = self.callPackage /home/sibi/github/persistent/persistent/default.nix {};
                    persistent = self.callPackage /home/sibi/github/persistent/persistent/default.nix {};
                    xmonad = self.callPackage /home/sibi/github/xmonad/default.nix {};
                    xmonad-contrib = self.callPackage /home/sibi/github/xmonad-contrib/default.nix {};
                    };
                    };
    sibiHaskellEnv = self.haskell.packages.ghc7102.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       # self.callPackage /home/sibi/github/xmonad/default.nix {}
                       # xmonad
                       xmonad              
                       xmonad-contrib
                       mtl
                       xmobar
                       http-client
                       http-conduit
                       conduit-combinators
                       setlocale
                     ]);
  };
}
