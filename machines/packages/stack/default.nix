{ pkgs }:
let
  libraryInputs = [ pkgs.zlib ];
  stackWrapped = pkgs.symlinkJoin {
    name = "stack";
    path = [ pkgs.stack];
    buildInputs = [ pkgs.makeWrapper pkgs.ghc ] ++ libraryInputs ;
    postBuild = ''
    wrapProgram $out/bin/stack --add-flags --no-nix --system-ghc --no-install-ghc
    '';
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libraryInputs;
  };
in stackWrapped
