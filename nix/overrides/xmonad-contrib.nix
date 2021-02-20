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
    sha256 = "0pm3riv60d6kakgv9yj57pzj3v2wn7gfdmhya63rjv8sw9d3b1yp";
    rev = "cff344811fabd18641d92b8352aa332aad89ad5f";
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
