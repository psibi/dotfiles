{ pkgs }:
let
  em = pkgs.writeScriptBin "em" ''
  #!${pkgs.stdenv.shell}

  # Pass explicit socket name for emacs provisioned via nixpkgs
  exec emacsclient -c --socket-name="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/emacs/server" "$@"
  '';
in em
