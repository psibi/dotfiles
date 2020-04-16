{ mkDerivation, alsa-mixer, base, bytestring, containers, hint
, libmpd, mtl, network, regex-posix, stdenv, X11, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-extras";
  version = "0.15.2";
  sha256 = "9a9ea86b49ce2fe9f4204d09bc5b32c323d42be8017429058b0568300bfb40dc";
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
