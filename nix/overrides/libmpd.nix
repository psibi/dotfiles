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
    sha256 = "0mmx0fj6sy9baywm45wws86xrbqbb4pznhv8ifrl0higi8ypnvvh";
    rev = "ac1a7f6492d50616588f01e0b107fb4baa617467";
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
