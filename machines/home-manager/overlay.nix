self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  terraform-ls = super.pkgs.callPackage ../packages/terraform-ls/default.nix {};
}
