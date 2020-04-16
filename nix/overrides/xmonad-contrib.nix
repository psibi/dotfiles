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
    sha256 = "07ldjqrbs03b2jsccmqlwdpxw5iq9fpj0ypjcdbi8fls8p61a20h";
    rev = "5521b432dd4106ba08073f18c93c6c142c2ad99b";
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
