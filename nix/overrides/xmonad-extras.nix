{ mkDerivation, alsa-mixer, base, bytestring, containers, hint
, libmpd, mtl, network, regex-posix, stdenv, X11, xmonad
, xmonad-contrib, fetchgit
}:
mkDerivation {
  pname = "xmonad-extras";
  version = "0.15.3";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-extras";
    sha256 = "1iwwhi1c52krvb6lvvwzbv4phxnvfy86rwvnd8b3fy6g41kgvan0";
    rev = "faae63b8adeeefd013a2652f4050695c2ab186e7";
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
