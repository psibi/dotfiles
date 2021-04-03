{ mkDerivation, base, bytestring, containers, directory
, extensible-exceptions, fetchgit, filepath, mtl, old-locale
, old-time, process, random, semigroups, stdenv, unix, utf8-string
, X11, X11-xft, xmonad
}:
mkDerivation {
  pname = "xmonad-contrib";
  version = "0.16";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-contrib";
    sha256 = "0000v4mrin9d3cg4z82q9gk9q4vkijbbi7mdld60f2hqdkd6vycy";
    rev = "654fa5045c87b4b71db730d8f4ef02687061c5b4";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers directory extensible-exceptions filepath
    mtl old-locale old-time process random semigroups unix utf8-string
    X11 X11-xft xmonad
  ];
  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = stdenv.lib.licenses.bsd3;
}
