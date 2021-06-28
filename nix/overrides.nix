pkgs: rec {
  # mumble = pkgs.mumble.override { pulseSupport = true; };
  keepassxc = pkgs.keepassxc.overrideAttrs (oldAttrs: {
    version = "2.5.4";
    src = pkgs.fetchFromGitHub {
      owner = "keepassxreboot";
      repo = "keepassxc";
      rev = "2.5.4";
      sha256 = "1xih9q1pxszalc0l29fmjxwn1vrrrrbnhc8gmi8brw5sclhbs6bh";
    };
    patches = [ ];
  });
  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      libmpd = haskellPackagesNew.callPackage ./overrides/libmpd.nix {};
      xmonad = let
        pkg = haskellPackagesNew.callPackage ./overrides/xmonad.nix { };
        in pkgs.haskell.lib.dontCheck pkg;
      xmonad-extras =
        haskellPackagesNew.callPackage ./overrides/xmonad-extras.nix { };
      xmonad-contrib = let
        pkg = haskellPackagesNew.callPackage ./overrides/xmonad-contrib.nix { };
        in pkgs.haskell.lib.dontCheck pkg;
      timezone-olson =
        haskellPackagesNew.callPackage ./overrides/timezone-olson.nix { };
      xmobar = let
        pkg = haskellPackagesNew.callPackage ./overrides/xmobar.nix {
        };
        in pkgs.haskell.lib.dontCheck pkg;
    };
  };
  sibi-xmobar = haskellPackages.callPackage ../xmobar/default.nix {
    xmobar = haskellPackages.xmobar;
  };
  sibi-xmonad = haskellPackages.callPackage ../xmonad/default.nix {
    xmonad-extras = haskellPackages.xmonad-extras;
    xmonad-contrib = haskellPackages.xmonad-contrib;
  };
}
