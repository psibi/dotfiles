{ pkgs }:
let
  em = pkgs.writeScriptBin "em" ''
  #!${pkgs.stdenv.shell}

  # Pass explicit socket name for emacs provisioned via nixpkgs
  exec emacsclient -c --socket-name=/run/user/1001/emacs/server "$@"
  '';
in em
