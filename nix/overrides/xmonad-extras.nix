{ mkDerivation, alsa-mixer, base, bytestring, containers, hint
, libmpd, mtl, network, regex-posix, stdenv, X11, xmonad
, xmonad-contrib, fetchgit
}:
mkDerivation {
  pname = "xmonad-extras";
  version = "0.15.3";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-extras";
    sha256 = "1kj8xzp7d8y0w63r46zvgav6a3320c6blsilaldaylgqb10h6aga";
    rev = "6df82de88474754bc90724251d5fcbeccccbd7e7";
    fetchSubmodules = false;
  };
  configureFlags = [
    "-f-with_hlist" "-fwith_parsec" "-fwith_split"
  ];
  libraryHaskellDepends = [
    alsa-mixer base bytestring containers hint libmpd mtl network
    regex-posix X11 xmonad xmonad-contrib
  ];
  homepage = "https://github.com/xmonad/xmonad-extras";
  description = "Third party extensions for xmonad with wacky dependencies";
  license = stdenv.lib.licenses.bsd3;
}
