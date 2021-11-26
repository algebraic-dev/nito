{ pkgs, ... }:

{
  system.stateVersion = "20.09";

  networking.firewall.allowedTCPPorts = [ 5432 ];

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    extraPlugins = with pkgs.postgresql.pkgs; [ postgis ];
    authentication = "host all all 10.233.0.0/16 trust";
    ensureDatabases = [ "nito" ];
    ensureUsers = [{
      name = "nito";
      ensurePermissions."DATABASE nito" = "ALL PRIVILEGES";
    }];
  };
}