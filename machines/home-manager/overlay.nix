self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { pkgs = super.pkgs; };
  flarectl = super.pkgs.callPackage ../packages/flarectl/default.nix { };
  # minijinja-cli = super.pkgs.callPackage ../packages/minijinja/default.nix { };
  sibi-yaml-language-server = super.pkgs.callPackage ../packages/yaml-language-server/default.nix {};
}
