inputs: { pkgs, config, lib, ... }:

with lib;

let

  cfg = config.services.auspex;

in
{

  options.services.auspex = {
    enable = mkEnableOption "auspex";

    port = mkOption {
      description = "Port to serve on";
      type = types.port;
      default = 8080;
    };

    openFirewall = mkOption {
      type = types.bool;
      description = "Whether to open firewall ports";
      default = false;
    };

    package = mkOption {
      type = types.package;
      description = "Package to run";
      default = inputs.self.packages.${pkgs.system}.default;
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [ cfg.port ];

    systemd.services.auspex = {
      description = "Auspex provider";
      wants = [ "network.target" ];
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        PORT = toString cfg.port;
      };

      script = ''
        mkdir -p data
        ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f ./data/ed25519 -N "" -q
        ${pkgs.openssl}/bin/openssl genpkey -algorithm RSA -out ./data/rsa -pkeyopt rsa_keygen_bits:2048 -quiet
        ${pkgs.openssl}/bin/openssl rsa -pubout -in ./data/rsa -out ./data/rsa.pub
        ${cfg.package}/bin/auspex
      '';

      serviceConfig = {
        RuntimeDirectory = "auspex";
        DynamicUser = true;
        WorkingDirectory = "/run/auspex";
        RestartSec = 15;
        User = "auspex";
        Restart = "on-failure";
      };
    };
  };

}
