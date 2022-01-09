{ config, pkgs, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> {};
in
 {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Custom systemd services
  imports = [ ../modules/cnx.nix ];
  services.cnx.enable = true;

  nixpkgs.config.allowUnfree = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sibi";
  home.homeDirectory = "/home/sibi";
  home.language = {
   monetary = "en_IN";
   time = "en_US.UTF-8";
  };
  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.cargo/bin"];
  home.sessionVariables = {
    EDITOR = "${pkgs.emacs}/bin/emacsclient";
  };

  services.emacs.enable = true;

  nixpkgs.config.packageOverrides = pkgs: rec {
    tfswitch = pkgs.callPackage ../packages/tfswitch/default.nix {};
    ouch = pkgs.callPackage ../packages/ouch/default.nix {};
    amber-secret = pkgs.callPackage ../packages/amber/default.nix {};
    tgswitch = pkgs.callPackage ../packages/tgswitch/default.nix {};
    cnx-sibi = pkgs.callPackage ../packages/cnx/default.nix {};
    kubergrunt = pkgs.callPackage ../packages/kubergrunt/default.nix {};
  };

  home.packages = import ./packages.nix { pkgs = pkgs; unstable = nixpkgs-unstable; };

  programs.git = {
    enable = true;
    userName = "Sibi Prabakaran";
    userEmail = "sibi@psibi.in";
    signing = {
      signByDefault = true;
      key = "BB557613";
    };
    ignores = [ "*~" "\#*\#" ".\#*"];
  };

  programs.fish = {
    enable = true;
    shellAliases = import ./alias.nix;
    interactiveShellInit = ''
    set fish_greeting
    '';
  };

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

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

  home.file.".stack/config.yaml".source = ../../../.stack/config.yaml;
  # home.file.".config/fish/config.fish".source = ../../.config/fish/config.fish;

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
