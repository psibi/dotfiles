{ mkDerivation, base, containers, stdenv, typed-process, X11
, xmonad, xmonad-contrib, xmonad-extras
}:
mkDerivation {
  pname = "sibi-xmonad";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers typed-process X11 xmonad xmonad-contrib
    xmonad-extras
  ];
 executableSystemDepends = [  ];
  homepage = "https://github.com/psibi/dotfiles";
  description = "Xmonad";
  license = stdenv.lib.licenses.bsd3;
}
