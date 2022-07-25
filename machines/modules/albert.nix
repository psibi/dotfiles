{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.albert;
in {
  options.services.albert = {
    enable = mkEnableOption "albert";
  };

  config = mkIf cfg.enable {
    systemd.user.services.albert = {
      Unit = {
        Description = "Albert application launcher";
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.albert}/bin/albert";
        Restart = "on-failure";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
