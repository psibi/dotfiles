{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.logid;
in
{
  options.services.logid = {
    enable = mkEnableOption "Logitech mouse daemon";

    configText = mkOption {
      type = types.lines;
      default = "";
      description = "logid configuration text";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.logiops ];

    environment.etc."logid.cfg".text = cfg.configText;

    systemd.services.logid = {
      description = "Logitech mouse daemon";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.logiops}/bin/logid";
        Restart = "on-failure";
        RestartSec = "1s";
      };
    };
  };
}
