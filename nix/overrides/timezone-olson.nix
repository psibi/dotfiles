{ mkDerivation, base, binary, bytestring, extensible-exceptions
, fetchgit, stdenv, time, timezone-series, lib
}:
mkDerivation {
  pname = "timezone-olson";
  version = "0.2.0";
  src = fetchgit {
    url = "https://github.com/ygale/timezone-olson";
    sha256 = "0rsydxh7cs6lsjp9izikcsvcljpd21nhk0m8mggls7njbimdj47f";
    rev = "8b9a86b1247dfb307ca41b7d8c25a1410ab1890c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base binary bytestring extensible-exceptions time timezone-series
  ];
  homepage = "http://projects.haskell.org/time-ng/";
  description = "A pure Haskell parser and renderer for binary Olson timezone files";
  license = lib.licenses.bsd3;
}
