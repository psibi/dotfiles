{ pkgs, unstable-pkgs, ... }: {
  # Custom systemd services
  imports = [ ../../modules/cnx.nix ];

  nixpkgs = {
    overlays = [ (import ../overlay.nix) ];
    config = {
      allowUnfreePredicate = (_: true);
      allowUnfree = true;
      documentation.dev.enable = true;
    };
  };

  home = {
    username = "sibi";
    homeDirectory = "/home/sibi";
    language = {
      monetary = "en_IN";
      time = "en_US.UTF-8";
    };
    sessionPath = [ "$HOME/.local/bin" "$HOME/.cargo/bin" ];
    sessionVariables = { EDITOR = "${pkgs.sibiEmacs}/bin/emacsclient"; };
    stateVersion = "21.05";

    packages = import ../packages.nix {
      pkgs = pkgs;
      unstable = unstable-pkgs;
    };

    file = {
      ".stack/config.yaml".source = ../../../.stack/config.yaml;
      ".tfswitch.toml".source = ../../../tfswitch.toml;
      ".tgswitch.toml".source = ../../../tgswitch.toml;
      ".aws/config".source = ../../../aws-config;
      ".config/mprocs/mprocs.yaml".source = ../../../mprocs.yaml;
    };
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
    lfs.enable = true;
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
    shellAliases = import ../alias.nix { pkgs = pkgs; };
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

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "ssh.dev.azure.com" = {
        user = "git";
        host = "ssh.dev.azure.com";
        hostname = "ssh.dev.azure.com";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
        extraOptions = {
          PubkeyAcceptedAlgorithms = "+ssh-rsa";
          HostkeyAlgorithms = "+ssh-rsa";
        };
      };
      "bitbucket" = {
        host = "bitbucket.org";
        hostname = "bitbucket.org";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
      };
      "bitbucket_bl3ndlabs" = {
        host = "bl3ndlabs";
        hostname = "bitbucket.org";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa_ed25519";
      };
      "github" = {
        host = "github.com";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
      };
      "github+levana" = {
        host = "github+levana";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = "~/.ssh/levana_id_rsa";
      };
      "gitlab" = {
        host = "gitlab.com";
        hostname = "gitlab.com";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
      };
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.obs-studio = {
    enable = true;
    package = pkgs.obs-studio;
    plugins = [ pkgs.obs-studio-plugins.obs-backgroundremoval ];
  };

  fonts.fontconfig.enable = true;

  services.cnx = {
    enable = true;
    machineName = "NUC";
  };

  services.emacs = {
    enable = true;
    package = pkgs.sibiEmacs;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60480000;
    defaultCacheTtlSsh = 60480000;
    maxCacheTtl = 60480000;
    maxCacheTtlSsh = 60480000;
    pinentryFlavor = "qt";
  };
}
