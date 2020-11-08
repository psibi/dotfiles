{ mkDerivation, base, hostname, hpack, stdenv, xmobar }:
mkDerivation {
  pname = "sibi-xmobar";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hostname xmobar ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base hostname xmobar];
  prePatch = "hpack";
  homepage = "https://github.com/psibi/dotfiles#readme";
  license = stdenv.lib.licenses.bsd3;
}
