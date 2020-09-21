self: super:
let
  unstable = builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/20.09-alpha.tar.gz";
in { emacs = (import unstable { }).emacs.override { withXwidgets = true; }; }

