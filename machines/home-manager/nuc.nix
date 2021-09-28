{ config, pkgs, ... }:

 {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sibi";
  home.homeDirectory = "/home/sibi";

  nixpkgs.config.packageOverrides = pkgs: rec {
    ouch = pkgs.callPackage ../packages/ouch/default.nix {};
  };

  home.packages = import ./packages.nix { pkgs = pkgs; };

  programs.git = {
    enable = true;
    userName = "sibi";
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
