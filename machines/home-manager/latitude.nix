{ config, pkgs, ... }:
let
  nixpkgs-unstable =
    import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Custom systemd services
  imports = [ ../modules/cnx.nix ];
  services.cnx = {
    enable = true;
    machineName = "LAPTOP";
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.documentation.dev.enable = true;

  nixpkgs.overlays = [ (import ./overlay.nix) ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sibi";
  home.homeDirectory = "/home/sibi";
  home.language = {
    monetary = "en_IN";
    time = "en_US.UTF-8";
  };
  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.cargo/bin" ];
  home.sessionVariables = {
    EDITOR = "${pkgs.emacs28NativeComp}/bin/emacsclient";
  };

  nixpkgs.config.packageOverrides = pkgs: rec {
    # ouch = pkgs.callPackage ../packages/ouch/default.nix {};
    # tfswitch = pkgs.callPackage ../packages/tfswitch/default.nix {};
    # amber-secret = pkgs.callPackage ../packages/amber/default.nix {};
    # tgswitch = pkgs.callPackage ../packages/tgswitch/default.nix {};
    cnx-sibi = pkgs.callPackage ../packages/cnx/default.nix { };
    # kubergrunt = pkgs.callPackage ../packages/kubergrunt/default.nix {};
    # jfmt = pkgs.callPackage ../packages/jfmt/default.nix {};
    # jless = pkgs.callPackage ../packages/jless/default.nix {};
  };

  home.packages = import ./packages.nix {
    pkgs = pkgs;
    unstable = nixpkgs-unstable;
  };

  programs.rofi = {
    enable = true;
    location = "center";
    pass.enable = false;
    theme = "fancy";
    extraConfig = { show-icons = false; };
  };

  programs.git = {
    enable = true;
    userName = "Sibi Prabakaran";
    userEmail = "sibi@psibi.in";
    signing = {
      signByDefault = true;
      key = "0xD19E3E0EBB557613";
    };
    extraConfig = {
      commit.gpgsign = true;
      init.defaultBranch = "main";
    };
    ignores = [ "*~" "#*#" ".#*" ];
  };

  programs.fish = {
    enable = true;
    shellAliases = import ./alias.nix { pkgs = pkgs; };
    interactiveShellInit = ''
      set fish_greeting
      ${pkgs.any-nix-shell}/bin/any-nix-shell fish | source
    '';
  };

  programs.gh = {
    enable = true;
    enableGitCredentialHelper = true;
  };

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
    package = nixpkgs-unstable.zoxide;
  };

  programs.direnv = { enable = true; };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = false;
    settings = {
      kubernetes = {
        format = "on [⛵ $context \\($namespace\\)](dimmed green) ";
        disabled = false;
      };
    };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = { family = "Ubuntu Mono"; };
        size = 17.0;
      };
      shell = { program = "${pkgs.screen}/bin/screen"; };
    };
  };

  programs.gpg = { enable = true; };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60480000;
    defaultCacheTtlSsh = 60480000;
    maxCacheTtl = 60480000;
    maxCacheTtlSsh = 60480000;
    pinentryFlavor = "qt";
  };

  fonts.fontconfig.enable = true;

  home.file.".stack/config.yaml".source = ../../.stack/config.yaml;
  home.file.".tfswitch.toml".source = ../../tfswitch.toml;
  home.file.".aws/config".source = ../../aws-config;
  home.file.".config/mprocs/mprocs.yaml".source = ../../mprocs.yaml;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
