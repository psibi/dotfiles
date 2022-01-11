{ mkDerivation, alsa-mixer, base, bytestring, containers, hint, lib
, libmpd, mtl, network, regex-posix, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-extras";
  version = "0.17.0";
  sha256 = "9d3a73472f3a65e2760cca25062cd3e96938ec39fe246e13db295c48d0b37c4e";
  revision = "1";
  editedCabalFile = "1ivm1nncg8qa5ww1kgk1d8zdawniz83ab5s2dj0hbcxffymgxp0k";
  configureFlags = [
    "-f-with_hlist" "-fwith_parsec" "-fwith_split"
  ];
  libraryHaskellDepends = [
    alsa-mixer base bytestring containers hint libmpd mtl network
    regex-posix X11 xmonad xmonad-contrib
  ];
  homepage = "https://github.com/xmonad/xmonad-extras";
  description = "Third party extensions for xmonad with wacky dependencies";
  license = lib.licenses.bsd3;
}
