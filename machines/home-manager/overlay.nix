self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  mprocs = super.pkgs.callPackage ../packages/mprocs/default.nix {};
  terraform-ls = super.pkgs.callPackage ../packages/terraform-ls/default.nix {};
}
