{ nixpkgs, pkgs, unstable-pkgs, lib, ... }:

{
  imports = [ ../../modules/cnx.nix ];

  nixpkgs = {
    overlays = [ (import ../overlay.nix ) ];
    config = {
      allowUnfreePredicate = (_: true);
      allowUnfree = true;
      documentation.dev.enable = true;
    };
  };

  nix.registry.nixpkgs.flake = nixpkgs;

  home = {
    username = "sibi";
    homeDirectory = "/home/sibi";
    language = {
      monetary = "en_IN";
      time = "en_US.UTF-8";
    };
    sessionPath = [ "$HOME/.local/bin" "$HOME/.cargo/bin" ];
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

  # https://discourse.nixos.org/t/set-default-application-for-mime-type-with-home-manager/17190/4
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = [ "google-chrome.desktop" ];
      "text/html" = [ "google-chrome.desktop" ];
      "x-scheme-handler/http" = [ "google-chrome.desktop" ];
      "x-scheme-handler/https" = [ "google-chrome.desktop" ];
      "x-scheme-handler/about" = [ "google-chrome.desktop" ];
      "x-scheme-handler/unknown" = [ "google-chrome.desktop" ];
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;

    config = {
      terminal = "alacritty";

      startup = [
        { command = "google-chrome-stable"; }
        { command = "alacritty"; }
        { command = "keepassxc"; }
      ];

      modifier = "Mod4"; # Super key
      input = {
        "type:keyboard" = {
          xkb_options = "caps:ctrl_modifier";
        };
      };

      window = {
        commands = [
          {
            command = "move scratchpad";
            criteria.app_id = "org.keepassxc.KeePassXC";
          }
          {
            command = "move container to workspace number 2";
            criteria.app_id = "google-chrome";
          }
          {
            command = "move container to workspace number 1";
            criteria.app_id = "Alacritty";
          }
        ];
      };

      keybindings =
        let modifier = "Mod4";
        in lib.mkOptionDefault {
          "${modifier}+Return" = "exec alacritty";
          "${modifier}+p" = "exec rofi -show run";
          "${modifier}+c" = "kill";

          "${modifier}+j" = "focus left";
          "${modifier}+k" = "focus down";
          "${modifier}+l" = "focus up";
          "${modifier}+Semicolon" = "focus right";

          "${modifier}+space" = "layout toggle tabbed splith splitv";

          "${modifier}+Ctrl+k" = "scratchpad show";

          "${modifier}+h" = "exec /home/sibi/.emacs_everywhere/bin/run";

          "${modifier}+Shift+z" = "exec ${pkgs.swaylock}/bin/swaylock \"--daemonize\"";
          "Print" = "exec flameshot gui";
        };

      bars = [{
        statusCommand = "i3status-rs /home/sibi/.config/i3status-rust/config-bottom.toml";
      }];
    };
  };

  programs.i3status-rust =
    {
      enable = true;
      bars = {
        bottom = {
          blocks = [
            {
              block = "disk_space";
              path = "/";
              info_type = "available";
              interval = 60;
              warning = 20.0;
              alert = 10.0;
            }
            {
              block = "battery";
              format = " $icon $percentage ";
            }
            {
              block = "memory";
              format = " $icon $mem_used_percents";
              interval = 30;
            }
            {
              block = "cpu";
              interval = 30;
            }
            {
              block = "net";
              interval = 60;
              device = "^wlp";
              format = " $icon {$signal_strength $ssid} $device";
            }
            {
              block = "load";
              interval = 60;
              format = " $icon $1m ";
            }
            { block = "sound"; }
            {
              block = "time";
              interval = 60;
              format = " $timestamp.datetime(f:'%a %d/%m/%Y %I:%M %p') ";
            }
          ];
          settings = {
            theme = {
              theme = "solarized-dark";
              overrides = {
                idle_bg = "#123456";
                idle_fg = "#abcdef";
              };
            };
          };
          icons = "awesome5";
          theme = "gruvbox-dark";
        };
      };
    };


  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.rofi = {
    enable = true;
    location = "center";
    pass.enable = false;
    theme = "fancy";
    extraConfig = { show-icons = false; };
    package = pkgs.rofi-wayland;
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
    gitCredentialHelper.enable = true;
  };

  programs.atuin = {
    enable = false;
    flags = [ "--disable-up-arrow" ];
    enableFishIntegration = false;
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
      shell = {
        program = "${pkgs.zellij}/bin/zellij";
        args = [
          "--config"
          "/home/sibi/github/dotfiles/zellij.kdl"
          "--layout"
          "/home/sibi/github/dotfiles/zellij_layout.kdl"
        ];
      };
    };
  };

  programs.gpg = { enable = true; };

  fonts.fontconfig.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60480000;
    defaultCacheTtlSsh = 60480000;
    maxCacheTtl = 60480000;
    maxCacheTtlSsh = 60480000;
    pinentryFlavor = "qt";
  };

  services.cnx = {
    enable = false;
    machineName = "LAPTOP";
  };

  services.emacs = {
    enable = true;
    package = pkgs.sibiEmacs;
    startWithUserSession = "graphical";
  };

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        disabledTrayIcon = true;
        showStartupLaunchMessage = false;
      };
    };
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
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
        identityFile = "~/.ssh/levana_key";
      };
      "gitlab" = {
        host = "gitlab.com";
        hostname = "gitlab.com";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa";
      };
    };
  };
}
