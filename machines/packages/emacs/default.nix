{ pkgs }:
pkgs.emacs29.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages; [
    vterm
    pdf-tools
    all-the-icons
    pkgs.emacsPackages.treesit-grammars.with-all-grammars
  ]))
