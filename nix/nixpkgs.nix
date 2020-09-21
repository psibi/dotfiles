import <nixpkgs> {
  config = import ./configuration.nix;
  overlays = [ (import ./emacs-overlay.nix) ];
}
