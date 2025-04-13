{ pkgs }:
pkgs.emacs-pgtk.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages; [
    vterm
    pdf-tools
    all-the-icons
    pkgs.emacsPackages.treesit-grammars.with-all-grammars
    jinx
  ]))
