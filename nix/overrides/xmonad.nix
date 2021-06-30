{ mkDerivation, base, containers, data-default, directory, fetchgit
, filepath, lib, mtl, process, QuickCheck, quickcheck-classes
, setlocale, transformers, unix, utf8-string, X11, typed-process
}:
mkDerivation {
  pname = "xmonad";
  version = "0.16.9999";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad.git";
    sha256 = "1kip5d7kyl740k96m0wx60v7ix7n0swgly84zrygdivr39g51d9j";
    rev = "42d319545b36020b9b3cdf121f1ba04cc58b847d";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default directory filepath mtl process
    setlocale transformers unix utf8-string X11 typed-process
  ];
  executableHaskellDepends = [ base mtl unix X11 ];
  testHaskellDepends = [
    base containers QuickCheck quickcheck-classes X11
  ];
  postInstall = ''
    install -D man/xmonad.1 ''${!outputDoc}/share/man/man1/xmonad.1
    install -D man/xmonad.hs ''${!outputDoc}/share/doc/$name/sample-xmonad.hs
  '';
  homepage = "http://xmonad.org";
  description = "A tiling window manager";
  license = lib.licenses.bsd3;
}
