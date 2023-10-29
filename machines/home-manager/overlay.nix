self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { pkgs = super.pkgs; };
  flarectl = super.pkgs.callPackage ../packages/flarectl/default.nix { };
}
