{ smos-docs-site
, smos-server
, smos-web-server
, opt-env-conf
, mkLooperOption
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
            config = mkOption {
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
              type = types.attrs;
              default = { };
            };
            port = mkOption {
              description = "The port to serve sync requests on";
              type = types.int;
              example = 8000;
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
            api-url = mkOption {
              description = "The url for the api server to refer to";
              type = types.nullOr types.str;
              default = null;
              example = "https://api.smos.online";
            };
            web-url = mkOption {
              description = "The url for the web server to refer to";
              type = types.nullOr types.str;
              default = null;
              example = "https://smos.online";
            };
            google-analytics-tracking = mkOption {
              description = "The Google analytics tracking code";
              type = types.nullOr types.str;
              example = "XX-XXXXXXXX-XX";
              default = null;
            };
            google-search-console-verification = mkOption {
              description = "The Google search console verification code";
              type = types.nullOr types.str;
              example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
              default = null;
            };
            pkg = mkOption {
              description = "The docs site package";
              type = types.package;
              default = smos-docs-site;
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
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      # The docs server
      docs-site-config = with cfg.docs-site; mergeListRecursively [
        (attrOrNull "port" port)
        (attrOrNull "api-url" api-url)
        (attrOrNull "web-url" (if builtins.isNull web-url then head hosts else web-url))
        (attrOrNull "google-analytics-tracking" google-analytics-tracking)
        (attrOrNull "google-search-console-verification" google-search-console-verification)
        cfg.docs-site.config
      ];
      docsSiteConfigFile = (pkgs.formats.yaml { }).generate "smos-docs-site-config.yaml" docs-site-config;
      docs-site-service =
        optionalAttrs (cfg.docs-site.enable or false) {
          "smos-docs-site-${envname}" =
            with cfg.docs-site;
            {
              description = "Smos docs site ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment =
                {
                  "SMOS_DOCS_SITE_CONFIG_FILE" = "${docsSiteConfigFile}";
                };
              script =
                ''
                  ${pkg}/bin/smos-docs-site
                '';
              serviceConfig =
                {
                  Restart = "always";
                  RestartSec = 1;
                  Nice = 15;
                };
              unitConfig =
                {
                  StartLimitIntervalSec = 0;
                  # ensure Restart=always is always honoured
                };
            };
        };
      docs-site-host =
        optionalAttrs ((cfg.docs-site.enable or false) && (cfg.docs-site.hosts or [ ]) != [ ]) {
          "${head cfg.docs-site.hosts}" =
            with cfg.docs-site;
            {
              enableACME = true;
              forceSSL = true;
              locations."/".proxyPass =
                "http://localhost:${builtins.toString port}";
              serverAliases = tail hosts;
            };
        };

      api-server-working-dir = working-dir + "api-server/";
      api-server-database-file = api-server-working-dir + "smos-server-database.sqlite3";
      api-server-config = mergeListRecursively [
        cfg.api-server.config
        cfg.api-server.extraConfig
      ];
      api-server-config-file = (pkgs.formats.yaml { }).generate "smos-api-server-config.yaml" api-server-config;
      # The api server
      api-server-service =
        optionalAttrs (cfg.api-server.enable or false) {
          "smos-api-server-${envname}" =
            timeZoneWarning {
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
            };
        };
      api-server-host =
        optionalAttrs ((cfg.api-server.enable or false) && (cfg.api-server.hosts or [ ]) != [ ]) {
          "${head cfg.api-server.hosts}" =
            with cfg.api-server;
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };

      # Local backup
      local-backup-service =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "smos-api-server-local-backup-${envname}" = {
                description = "Backup smos-api-server database locally for ${envname}";
                wantedBy = [ ];
                script =
                  ''
                    mkdir -p ${backup-dir}
                    cd ${working-dir}
                    file="${backup-dir}/''$(date +%F_%T).db"
                    ${pkgs.sqlite}/bin/sqlite3 ${api-server-database-file} ".backup ''${file}"
                  '';
                serviceConfig = {
                  Type = "oneshot";
                };
              };
            }
          )
        );
      local-backup-timer =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "smos-api-server-local-backup-${envname}" = {
                description = "Backup smos-api-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );

      # The web server
      web-server-working-dir = working-dir + "web-server/";
      web-server-data-dir = web-server-working-dir + "web-server/";
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
          "${head cfg.web-server.hosts}" =
            with cfg.web-server;
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # To make the websockets api work
                proxyWebsockets = true;
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
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
        (optional ((cfg.docs-site.enable or false) && cfg.docs-site.openFirewall) cfg.docs-site.port)
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
