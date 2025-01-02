{ nixpkgs, pkgs, unstable-pkgs, lib, ... }:
{
  # Custom systemd services
  imports = [ ];

  nixpkgs = {
    overlays = [ (import ../overlay.nix) ];
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
    sessionVariables = {
      EDITOR = "${pkgs.sibiEmacs}/bin/emacsclient";
      NIXOS_OZONE_WL = "1";
    };
    stateVersion = "21.05";

    packages = import ../packages.nix {
      pkgs = pkgs;
      unstable = unstable-pkgs;
    };

    file = {
      ".stack/config.yaml".source = ../../../.stack/config.yaml;
      ".stack/global-project/stack.yaml".source = ../../../.stack/stack.yaml;
      ".tfswitch.toml".source = ../../../tfswitch.toml;
      ".tgswitch.toml".source = ../../../tgswitch.toml;
      ".aws/config".source = ../../../aws-config;
      ".config/mprocs/mprocs.yaml".source = ../../../mprocs.yaml;
    };
  };

  programs.rofi = {
    package = pkgs.rofi-wayland;
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
    gitCredentialHelper.enable = true;
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
      custom.aws = {
        when = "printenv AWS_REGION";
        command = "aws-helper info";
        ignore_timeout = true;
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
      terminal = {
        shell = {
          program = "${unstable-pkgs.zellij}/bin/zellij";
          args = [
            "--config"
            "/home/sibi/github/dotfiles/zellij.kdl"
            "--layout"
            "/home/sibi/github/dotfiles/zellij_layout.kdl"
          ];
        };
      };
    };
  };

  programs.gpg = { enable = true; };

  programs.ssh = {
    enable = true;
    serverAliveInterval = 15;
    serverAliveCountMax = 3;
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

  # https://discourse.nixos.org/t/set-default-application-for-mime-type-with-home-manager/17190/4
  # ls ~/.nix-profile/share/applications/
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = [ "chromium.desktop" ];
      "text/html" = [ "chromium.desktop" ];
      "x-scheme-handler/http" = [ "chromium.desktop" ];
      "x-scheme-handler/https" = [ "chromium.desktop" ];
      "x-scheme-handler/about" = [ "chromium.desktop" ];
      "x-scheme-handler/unknown" = [ "chromium.desktop" ];
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = [ "writer.desktop" ];
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk ];
    config = { sway.default = [ "wlr" "gtk" ]; };
  };

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;

    config = {
      terminal = "alacritty";

      startup = [
        { command = "chromium"; }
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
            command = "move scratchpad; scratchpad show";
            criteria.app_id = "org.keepassxc.KeePassXC";
          }
          {
            command = "move container to workspace number 2";
            criteria.app_id = "chromium-browser";
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
              block = "memory";
              format = " $icon $mem_used_percents";
              interval = 30;
            }
            {
              block = "weather";
              format = "$icon $weather $temp";
              # Uncomment on i3status 0.32.3
              # format_alt = " $icon_ffin Forecast (9 hour avg) {$temp_favg ({$temp_fmin}-{$temp_fmax})|Unavailable} ";
              interval = 6000;
              service = {
                name = "metno";
                coordinates = [ "12.9923" "77.7161" ];
                forecast_hours = 9;
              };
            }
            {
              block = "temperature";
              interval = 15;
              good = 50;
              info = 60;
              warning = 80;
              chip = "k10temp-pci-00c3";
              inputs = [ "Tccd1" "Tccd2" ];
              format = " $icon $max";
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

  programs.obs-studio = {
    enable = true;
    package = pkgs.obs-studio;
    plugins = [ pkgs.obs-studio-plugins.obs-backgroundremoval ];
  };

  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.zellij = {
    enable = true;
    enableFishIntegration = true;
  };

  fonts.fontconfig.enable = true;

  services.emacs = {
    enable = true;
    package = pkgs.sibiEmacs;
    startWithUserSession = "graphical";
  };

  services.swayidle = {
    enable = true;
    events = [
    ];
    timeouts = [
      {
        timeout = 1800;
        command = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        resumeCommand = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
      }
      {
        timeout = 1780;
        command = "${pkgs.swaylock}/bin/swaylock \"--daemonize\"";
      }
    ];
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60480000;
    defaultCacheTtlSsh = 60480000;
    maxCacheTtl = 60480000;
    maxCacheTtlSsh = 60480000;
    pinentryPackage = pkgs.pinentry-qt;
  };

  services.flameshot = {
    enable = true;
    package = pkgs.flameshot-grim;
    settings = {
      General = {
        disabledTrayIcon = true;
        showStartupLaunchMessage = false;
      };
    };
  };

}
