self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { pkgs = super.pkgs; };
  # flarectl = super.pkgs.callPackage ../packages/flarectl/default.nix { };
  # tombi = super.pkgs.callPackage ../packages/tombi/default.nix {};
  # spl-token-cli = super.pkgs.callPackage ../packages/spl-token-cli/default.nix {};
  # gemini-cli = super.pkgs.callPackage ../packages/gemini-cli/default.nix {};
  # minijinja-cli = super.pkgs.callPackage ../packages/minijinja/default.nix { };
  sibi-yaml-language-server = super.pkgs.callPackage ../packages/yaml-language-server/default.nix {};
  sibi-goose-cli = super.pkgs.callPackage ../packages/goose-cli/package.nix {};
}
