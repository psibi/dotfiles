{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.cnx;
in {
  options.services.cnx = {
    enable = mkEnableOption "cnx";
  };

  config = mkIf cfg.enable {
    systemd.user.services.cnx = {
      Unit = {
        Description = "Cnx status bar";
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.cnx-sibi}/bin/cnx-sibi";
        Restart = "on-failure";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };
  };
}
