{ mkDerivation, base, bytestring, containers, directory
, extensible-exceptions, fetchgit, filepath, mtl, old-locale
, old-time, process, random, semigroups, stdenv, unix, utf8-string
, X11, X11-xft, xmonad, lib
}:
mkDerivation {
  pname = "xmonad-contrib";
  version = "0.17";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-contrib";
    sha256 = "1sklqw79azpc6wdgyfiaqdamgw6nqccw5y5jgmpf9z59z59xhzk5";
    rev = "1351f9a931f53e9f1e16c566c70cb8fa98f97785";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers directory extensible-exceptions filepath
    mtl old-locale old-time process random semigroups unix utf8-string
    X11 X11-xft xmonad
  ];
  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = lib.licenses.bsd3;
}
