{pkgs}: (self: super: {
  em = import ./scripts/em.nix { pkgs = pkgs; };
})
