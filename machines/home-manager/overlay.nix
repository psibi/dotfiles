{ unstable}: self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { unstable = unstable; };
}
