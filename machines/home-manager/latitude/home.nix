{ nixpkgs, pkgs, unstable-pkgs, lib, ... }:

{
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
      # "application/pdf" = [ "firefox.desktop" ];
      # "text/html" = [ "firefox.desktop" ];
      # "x-scheme-handler/http" = [ "firefox.desktop" ];
      # "x-scheme-handler/https" = [ "firefox.desktop" ];
      # "x-scheme-handler/about" = [ "firefox.desktop" ];
      # "x-scheme-handler/unknown" = [ "firefox.desktop" ];
      "application/pdf" = [ "google-chrome.desktop" ];
      "text/html" = [ "google-chrome.desktop" ];
      "x-scheme-handler/http" = [ "google-chrome.desktop" ];
      "x-scheme-handler/https" = [ "google-chrome.desktop" ];
      "x-scheme-handler/about" = [ "google-chrome.desktop" ];
      "x-scheme-handler/unknown" = [ "google-chrome.desktop" ];
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
      terminal = "ghostty";

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
        "type:touchpad" = {
          tap = "enabled";
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
            criteria.app_id = "google-chrome";
          }
          {
            command = "move container to workspace number 1";
            criteria.app_id = "com.mitchellh.ghostty";
          }
        ];
      };

      keybindings =
        let modifier = "Mod4";
        in lib.mkOptionDefault {
          "${modifier}+Return" = "exec ghostty";
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
              block = "weather";
              format = "$icon $weather $temp";
              format_alt = " $icon_ffin Forecast (9 hour avg) {$temp_favg ({$temp_fmin}-{$temp_fmax})|Unavailable} ";
              interval = 6000;
              service = {
                name = "metno";
                coordinates = [ "12.9923" "77.7161" ];
                forecast_hours = 9;
              };
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
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        email = "sibi@psibi.in";
        name = "Sibi Prabakaran";
      };
      init.defaultBranch = "main";
    };
    signing = {
      signByDefault = true;
      key = "0xD19E3E0EBB557613";
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
      custom.aws = {
        when = "printenv AWS_REGION";
        command = "aws-helper info";
        ignore_timeout = true;
      };
    };
  };

  programs.ghostty = {
    enable = true;
    enableFishIntegration = true;
    package = pkgs.ghostty;
    settings = {
      font-family = "Ubuntu Mono";
      font-size = "17.0";
      command = "fish";
      window-decoration = "false";
      theme = "Dark Modern";
      initial-command = "zellij --config /home/sibi/github/dotfiles/zellij.kdl --layout /home/sibi/github/dotfiles/zellij_layout.kdl";
      shell-integration = "fish";
      cursor-style = "block";
      cursor-style-blink = "false";
      window-theme = "ghostty";
      shell-integration-features = "no-cursor";
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
    pinentry.package = pkgs.pinentry-qt;
  };

  services.emacs = {
    enable = true;
    package = pkgs.sibiEmacs;
    defaultEditor = true;
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

  services.mako.enable = true;

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        serverAliveInterval = 60;
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
