{ mkDerivation, base, containers, data-default, directory
, extensible-exceptions, fetchgit, filepath, mtl, process
, QuickCheck, setlocale, stdenv, unix, utf8-string, X11
}:
mkDerivation {
  pname = "xmonad";
  version = "0.15";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad";
    sha256 = "1rjaw9xvjk59ia0h097sckgdmq6i9a9cdigm9q1i3x0af5zqvnyg";
    rev = "ff738988d382c82ae7720b2149e97bd2c8402377";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default directory extensible-exceptions
    filepath mtl process setlocale unix utf8-string X11
  ];
  executableHaskellDepends = [ base mtl unix X11 ];
  testHaskellDepends = [
    base containers extensible-exceptions QuickCheck X11
  ];
  postInstall = ''
    install -D man/xmonad.1 ''${!outputDoc}/share/man/man1/xmonad.1
    install -D man/xmonad.hs ''${!outputDoc}/share/doc/$name/sample-xmonad.hs
  '';
  homepage = "http://xmonad.org";
  description = "A tiling window manager";
  license = stdenv.lib.licenses.bsd3;
}
