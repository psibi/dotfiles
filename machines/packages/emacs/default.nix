{ unstable, pkgs }:
unstable.emacs29.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages; [
    vterm
    pdf-tools
    all-the-icons
    unstable.emacsPackages.treesit-grammars.with-all-grammars
  ]))
