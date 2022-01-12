{ mkDerivation, base, bytestring, containers, deepseq, directory
, fetchgit, filepath, hspec, lib, mtl, process, QuickCheck, random
, time, unix, utf8-string, X11, X11-xft, xmonad
}:
mkDerivation {
  pname = "xmonad-contrib";
  version = "0.17.0.9";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-contrib.git";
    sha256 = "00hr16dcb1a9mc65xc9nz2qyxxbr6fcmbffh4psc1c123w3ng91p";
    rev = "41ca79986bc3b29f5092ea54104c4d90223a07dd";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers deepseq directory filepath mtl process
    random time unix utf8-string X11 X11-xft xmonad
  ];
  testHaskellDepends = [
    base containers deepseq directory hspec mtl process QuickCheck
    random time unix utf8-string X11 xmonad
  ];
  homepage = "https://xmonad.org/";
  description = "Community-maintained extensions extensions for xmonad";
  license = lib.licenses.bsd3;
}
