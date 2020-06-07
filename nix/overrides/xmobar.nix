{ mkDerivation, alsa-core, alsa-mixer, async, base, bytestring
, containers, dbus, directory, extensible-exceptions, filepath
, hinotify, hspec, http-conduit, http-types, iwlib, libmpd, libXpm
, libXrandr, libXrender, mtl, old-locale, parsec, parsec-numbers
, process, regex-compat, stdenv, stm, temporary, time
, timezone-olson, timezone-series, transformers, unix, utf8-string
, wirelesstools, X11, X11-xft, fetchgit
}:
mkDerivation {
  pname = "xmobar";
  version = "0.33";
  src = fetchgit {
    url = "https://github.com/psibi/xmobar";
    sha256 = "1mrn29dzxznramlzljnp9cd1xxf95ly1ffwd1v95a089rv11pfjg";
    rev = "56bea57b9e9fffe0ea9dda4ea44102285316ec30";
    fetchSubmodules = true;
  };  
  configureFlags = [
    "-fwith_alsa" "-fwith_conduit" "-fwith_datezone"
    "-fwith_iwlib" "-fwith_rtsopts" "-fwith_threaded" "-fwith_utf8" 
    "-fwith_weather" "-fwith_xft" "-fwith_xpm"
    # "-fwith_dbus" "-fwith_inotify" "-fwith_mpd" "-fwith_mpris"
    # "-fwith_uvmeter"
  ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    alsa-core alsa-mixer async base bytestring containers dbus
    directory extensible-exceptions filepath hinotify http-conduit
    http-types iwlib libmpd mtl old-locale parsec parsec-numbers
    process regex-compat stm time timezone-olson timezone-series
    transformers unix utf8-string X11 X11-xft
  ];
  librarySystemDepends = [
    libXpm libXrandr libXrender wirelesstools
  ];
  executableHaskellDepends = [
    async base containers directory filepath parsec unix X11
  ];
  testHaskellDepends = [
    alsa-core alsa-mixer async base bytestring containers directory
    filepath hspec mtl old-locale parsec parsec-numbers process
    regex-compat stm temporary time transformers unix X11
  ];
  homepage = "http://xmobar.org";
  description = "A Minimalistic Text Based Status Bar";
  license = stdenv.lib.licenses.bsd3;
}
