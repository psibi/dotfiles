{ pkgs }:
pkgs.emacs28NativeComp.pkgs.withPackages (epkgs:
  (with epkgs.melpaPackages; [
    vterm
    tree-sitter
    pdf-tools
    (epkgs.tree-sitter-langs.withPlugins
      (p: epkgs.tree-sitter-langs.plugins ++ [ p.tree-sitter-markdown ]))
    all-the-icons
  ]))
