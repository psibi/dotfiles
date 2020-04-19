{ mkDerivation, attoparsec, base, bytestring, directory, filepath
, rio, stdenv, text
}:
mkDerivation {
  pname = "utils";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring directory filepath rio text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/psibi/dotfiles#readme";
  license = stdenv.lib.licenses.bsd3;
}
