{ smos-docs-site
, smos-server
, smos-web-server
, opt-env-conf
}:
{ envname
}:
{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.smos."${envname}";

  timeZoneWarning = warnIf (builtins.isNull config.time.timeZone) ''
    The smos-server executable cannot produce reports unless time.timeZone is
    set.
    It is currently set to null, so some requests will result in a 5XX error.
  '';

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.smos."${envname}" =
    {
      enable = mkEnableOption "Smos Service";
      docs-site = mkOption {
        default = null;
        description = "Smos' documentation site service";
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Smos Docs Site";
            pkg = mkOption {
              description = "The docs site package";
              type = types.package;
              default = smos-docs-site;
            };
            openFirewall = mkOption {
              type = types.bool;
              default = false;
              description = "Whether to open the specified port in the firewall";
            };
            hosts = mkOption {
              description = "The host to serve the docs site on";
              type = types.listOf types.str;
              default = [ ];
              example = [ "docs.smos.online" ];
            };
            config = mkOption {
              default = { };
              description = "Typed contents of the config file";
              type = types.submodule {
                options = import ../smos-docs-site/options.nix { inherit lib; };
              };
            };
            extraConfig = mkOption {
              description = "Extra contents of the config file";
              default = { };
            };
          };
        });
      };
      api-server = mkOption {
        default = null;
        description = "Smos' API server service";
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Smos API Server";
            pkg = mkOption {
              description = "The docs server package";
              type = types.package;
              default = smos-server;
            };
            hosts = mkOption {
              description = "The host to serve api requests on";
              type = types.listOf types.str;
              default = [ ];
              example = [ "api.smos.online" ];
            };
            openFirewall = mkOption {
              type = types.bool;
              default = false;
              description = "Whether to open the specified port in the firewall";
            };
            config = mkOption {
              default = { };
              description = "Typed contents of the config file";
              type = types.submodule {
                options = import ../smos-server-gen/options.nix { inherit lib; };
              };
            };
            extraConfig = mkOption {
              description = "Extra contents of the config file";
              default = { };
            };
          };
        });
      };
      web-server = mkOption {
        default = null;
        description = "Smos' web server service";
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Smos Web Server";
            pkg = mkOption {
              description = "The web server package";
              type = types.package;
              default = smos-web-server;
            };
            hosts = mkOption {
              description = "The host to serve web requests on";
              type = types.listOf types.str;
              default = [ ];
              example = [ "smos.online" ];
            };
            openFirewall = mkOption {
              type = types.bool;
              default = false;
              description = "Whether to open the specified port in the firewall";
            };
            config = mkOption {
              default = { };
              description = "Typed contents of the config file";
              type = types.submodule {
                options = import ../smos-web-server/options.nix { inherit lib; };
              };
            };
            extraConfig = mkOption {
              description = "Extra contents of the config file";
              default = { };
            };
          };
        });
      };
    };
  config =
    let
      working-dir = "/www/smos/${envname}/";
      # The docs server
      docs-site-config = mergeListRecursively [
        cfg.docs-site.config
        cfg.docs-site.extraConfig
      ];
      docs-site-config-file = (pkgs.formats.yaml { }).generate "smos-docs-site-config.yaml" docs-site-config;
      docs-site-service =
        optionalAttrs (cfg.docs-site.enable or false) {
          "smos-docs-site-${envname}" = opt-env-conf.addSettingsCheckToService {
            description = "Smos docs site ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment = {
              "SMOS_DOCS_SITE_CONFIG_FILE" = "${docs-site-config-file}";
            };
            script = ''
              ${cfg.docs-site.pkg}/bin/smos-docs-site
            '';
            serviceConfig = {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
            unitConfig = {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
          };
        };
      docs-site-host =
        optionalAttrs ((cfg.docs-site.enable or false) && (cfg.docs-site.hosts or [ ]) != [ ]) {
          "${head cfg.docs-site.hosts}" = {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://localhost:${builtins.toString cfg.docs-site.config.port}";
            serverAliases = tail cfg.docs-site.hosts;
          };
        };

      api-server-working-dir = working-dir + "api-server/";
      api-server-config = mergeListRecursively [
        cfg.api-server.config
        cfg.api-server.extraConfig
      ];
      api-server-config-file = (pkgs.formats.yaml { }).generate "smos-api-server-config.yaml" api-server-config;
      # The api server
      api-server-service =
        optionalAttrs (cfg.api-server.enable or false) {
          "smos-api-server-${envname}" = timeZoneWarning (opt-env-conf.addSettingsCheckToService {
            description = "Smos API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment = {
              "SMOS_SERVER_CONFIG_FILE" = "${api-server-config-file}";
            };
            script = ''
              mkdir -p "${api-server-working-dir}"
              cd ${api-server-working-dir}
              ${cfg.api-server.pkg}/bin/smos-server
            '';
            serviceConfig = {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
            unitConfig = {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
          });
        };
      api-server-host =
        optionalAttrs ((cfg.api-server.enable or false) && (cfg.api-server.hosts or [ ]) != [ ]) {
          "${head cfg.api-server.hosts}" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://localhost:${builtins.toString cfg.api-server.config.port}";
              # Just to make sure we don't run into 413 errors on big syncs
              extraConfig = ''
                client_max_body_size 0;
              '';
            };
            serverAliases = tail cfg.api-server.hosts;
          };
        };

      # The web server
      web-server-working-dir = working-dir + "web-server/";
      web-server-config = mergeListRecursively [
        cfg.web-server.config
        cfg.web-server.extraConfig
      ];
      web-server-config-file = (pkgs.formats.yaml { }).generate "smos-web-server-config.yaml" web-server-config;
      web-server-service =
        optionalAttrs (cfg.web-server.enable or false) {
          "smos-web-server-${envname}" = opt-env-conf.addSettingsCheckToService {
            description = "Smos web server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment = {
              "SMOS_WEB_SERVER_CONFIG_FILE" = "${web-server-config-file}";
              "TERM" = "xterm-256color";
            };
            script = ''
              mkdir -p "${web-server-working-dir}"
              cd ${web-server-working-dir};
              ${cfg.web-server.pkg}/bin/smos-web-server
            '';
            serviceConfig = {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
            unitConfig = {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
          };
        };
      web-server-host =
        optionalAttrs ((cfg.web-server.enable or false) && (cfg.web-server.hosts or [ ]) != [ ]) {
          "${head cfg.web-server.hosts}" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://localhost:${builtins.toString cfg.web-server.config.port}";
              # To make the websockets api work
              proxyWebsockets = true;
              # Just to make sure we don't run into 413 errors on big syncs
              extraConfig = ''
                client_max_body_size 0;
              '';
            };
            serverAliases = tail cfg.web-server.hosts;
          };
        };
    in
    mkIf (cfg.enable or false) {
      systemd.services = mergeListRecursively [
        docs-site-service
        api-server-service
        web-server-service
      ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional ((cfg.docs-site.enable or false) && cfg.docs-site.openFirewall) cfg.docs-site.config.port)
        (optional ((cfg.api-server.enable or false) && cfg.api-server.openFirewall) cfg.api-server.config.port)
        (optional ((cfg.web-server.enable or false) && cfg.web-server.openFirewall) cfg.web-server.config.port)
      ];
      services.nginx.virtualHosts = mergeListRecursively [
        docs-site-host
        api-server-host
        web-server-host
      ];
    };
}
