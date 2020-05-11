{ mkDerivation, attoparsec, base, bytestring, conduit
, conduit-extra, directory, filepath, MissingH
, optparse-applicative, resourcet, shell-conduit, split, stdenv
, text, unzip
}:
mkDerivation {
  pname = "utils";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring conduit conduit-extra directory filepath
    MissingH resourcet shell-conduit split text
  ];
  libraryToolDepends = [ unzip];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/psibi/dotfiles#readme";
  license = stdenv.lib.licenses.bsd3;
}
