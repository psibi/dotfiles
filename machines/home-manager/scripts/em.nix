{ pkgs }:
let
  em = pkgs.writeScriptBin "emm" ''
  #!${pkgs.stdenv.shell}

  # Pass explicit socket name for emacs provisioned via nixpkgs
  exec emacsclient -c --socket-name=/run/user/1000/emacs/server "$@"
  '';
in em
