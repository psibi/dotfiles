{ config, pkgs, ... }:

 {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sibi";
  home.homeDirectory = "/home/sibi";
  home.language = {
   monetary = "en_IN";
   time = "en_US.UTF-8";
  };
  home.sessionPath = [ "~/.local/bin" ];

  nixpkgs.config.packageOverrides = pkgs: rec {
    tfswitch = pkgs.callPackage ../packages/tfswitch/default.nix {};
    ouch = pkgs.callPackage ../packages/ouch/default.nix {};
    amber-secret = pkgs.callPackage ../packages/amber/default.nix {};
    tgswitch = pkgs.callPackage ../packages/tgswitch/default.nix {};
  };

  home.packages = import ./packages.nix { pkgs = pkgs; };

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
