self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  terraform-ls = super.pkgs.callPackage ../packages/terraform-ls/default.nix { };
  cnx-sibi = super.pkgs.callPackage ../packages/cnx/default.nix { };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { };
}
