{ mkDerivation, attoparsec, base, bytestring, containers
, data-default-class, fetchgit, filepath, hspec, mtl, network
, QuickCheck, safe-exceptions, stdenv, text, time, unix
, utf8-string
}:
mkDerivation {
  pname = "libmpd";
  version = "0.9.2.0";
  src = fetchgit {
    url = "https://github.com/vimus/libmpd-haskell";
    sha256 = "1p2xcpsgcdxdk0f8zcy8rq7p0lv73iw366jgkpxprxaxqbhj487v";
    rev = "1ec02deba33ce2a16012d8f0954e648eb4b5c485";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    attoparsec base bytestring containers data-default-class filepath
    mtl network safe-exceptions text time utf8-string
  ];
  testHaskellDepends = [
    attoparsec base bytestring containers data-default-class filepath
    hspec mtl network QuickCheck safe-exceptions text time unix
    utf8-string
  ];
  homepage = "http://github.com/vimus/libmpd-haskell#readme";
  description = "An MPD client library";
  license = stdenv.lib.licenses.mit;
}
