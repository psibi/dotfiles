{ pkgs }:
pkgs.emacs29-pgtk.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages; [
    vterm
    pdf-tools
    all-the-icons
    pkgs.emacsPackages.treesit-grammars.with-all-grammars
    jinx
  ]))
