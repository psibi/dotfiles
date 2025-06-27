self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  sibiEmacs = super.pkgs.callPackage ../packages/emacs/default.nix { pkgs = super.pkgs; };
  flarectl = super.pkgs.callPackage ../packages/flarectl/default.nix { };
  tombi = super.pkgs.callPackage ../packages/tombi/default.nix {};
  gemini-cli = super.pkgs.callPackage ../packages/gemini-cli/default.nix {};
  # minijinja-cli = super.pkgs.callPackage ../packages/minijinja/default.nix { };
  sibi-yaml-language-server = super.pkgs.callPackage ../packages/yaml-language-server/default.nix {};
  flameshot-grim = super.pkgs.flameshot.overrideAttrs (oldAttrs: {
    src = super.pkgs.fetchFromGitHub {
      owner = "flameshot-org";
      repo = "flameshot";
      rev = "3d21e4967b68e9ce80fb2238857aa1bf12c7b905";
      sha256 = "sha256-OLRtF/yjHDN+sIbgilBZ6sBZ3FO6K533kFC1L2peugc=";
    };
    cmakeFlags = [
      "-DUSE_WAYLAND_CLIPBOARD=1"
      "-DUSE_WAYLAND_GRIM=1"
    ];
    buildInputs = oldAttrs.buildInputs ++ [ super.pkgs.libsForQt5.kguiaddons ];
  });
}
