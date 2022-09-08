{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.cloudflare-warp;

in {
  meta.maintainers = with maintainers; [ wolfangaukang ];

  options = {
    services.cloudflare-warp = {
      enable = mkEnableOption "cloudflare-warp, a service that replaces the connection between your device and the Internet with a modern, optimized, protocol";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ cloudflare-warp ];

    users.users.warp = {
      isSystemUser = true;
      group = "warp";
      description = "Cloudflare Warp user";
      home = "/var/lib/cloudflare-warp";
    };
    users.groups.warp = {};

    systemd = {
      packages = [ pkgs.cloudflare-warp ];
      services.warp-svc = {
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Environment = [ "RUST_LOG=INFO"];
          StateDirectory = "cloudflare-warp";
          User = "warp";
          Umask = "0077";
          # Hardening
          LockPersonality = true;
          PrivateMounts = true;
          PrivateTmp = true;
          ProtectControlGroups = true;
          ProtectHostname = true;
          ProtectKernelLogs = true;
          ProtectKernelModules = true;
          ProtectKernelTunables = true;
          ProtectProc = "invisible";
          # Leaving on strict activates warp on plus
          ProtectSystem = "full";
          RestrictNamespaces = true;
          RestrictRealtime = true;
        };
      };
    };
  };
}
