{ envname
, sources ? import ./sources.nix
, smosPackages ? (import ./pkgs.nix { inherit sources; }).smosPackages
}:

{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.smos."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };

  mkLooperOption = pkgs.callPackage (sources.looper + "/nix/looper-option.nix") { };
in
{
  options.services.smos."${envname}" =
    {
      enable = mkEnableOption "Smos Service";
      docs-site =
        mkOption {
          default = null;
          type =
            types.nullOr (types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos Docs Site";
                  config =
                    mkOption {
                      description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                      default = { };
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8000;
                      description = "The port to serve sync requests on";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      example = [ "docs.smos.online" ];
                      description = "The host to serve the docs site on";
                    };
                  api-url =
                    mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "https://api.smos.online";
                      description = "The url for the api server to refer to";
                    };
                  web-url =
                    mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "https://smos.online";
                      description = "The url for the web server to refer to";
                    };
                  google-analytics-tracking =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XX-XXXXXXXX-XX";
                      default = null;
                      description = "The Google analytics tracking code";
                    };
                  google-search-console-verification =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                      default = null;
                      description = "The Google search console verification code";
                    };
                  pkg =
                    mkOption {
                      default = smosPackages.smos-docs-site;
                      description = "The docs site package";
                    };
                };
            });
        };
      api-server =
        mkOption {
          default = null;
          type =
            types.nullOr (types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos API Server";
                  config =
                    mkOption {
                      description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                      default = { };
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve api requests on";
                    };
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "Debug";
                      default = "Warn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      example = "api.smos.online";
                      description = "The host to serve api requests on";
                    };
                  admin =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "admin";
                      default = null;
                      description = "The username of the admin user";
                    };
                  max-backups-per-user =
                    mkOption {
                      type = types.nullOr types.int;
                      default = null;
                      example = 5;
                      description = "The maximum number of backups per user";
                    };
                  max-backup-size-per-user =
                    mkOption {
                      type = types.nullOr types.int;
                      default = null;
                      example = 1024 * 1024;
                      description = "The maximum number of bytes that backups can take up per user";
                    };
                  local-backup =
                    mkOption {
                      type = types.nullOr (
                        types.submodule {
                          options = {
                            enable = mkEnableOption "Smos API Server Local Backup Service";
                            backup-dir = mkOption {
                              type = types.str;
                              example = "backup/api-server";
                              default = "backup/api-server";
                              description = "The directory to store backups in, relative to the /www/smos/${envname} directory or absolute";
                            };
                          };
                        }
                      );
                      default = null;
                    };
                  auto-backup = mkLooperOption "auto-backup";
                  backup-garbage-collector = mkLooperOption "backup-garbage-collector";
                  pkg =
                    mkOption {
                      default = smosPackages.smos-server;
                      description = "The docs server package";
                    };
                };
            });
        };
      web-server =
        mkOption {
          default = null;
          type =
            types.nullOr (types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos Web Server";
                  config =
                    mkOption {
                      description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                      default = { };
                    };
                  docs-url =
                    mkOption {
                      type = types.str;
                      example = "docs.smos.online";
                      default = "docs.smos.online";
                      description = "The url for the docs to refer to";
                    };
                  api-url =
                    mkOption {
                      type = types.str;
                      example = "api.smos.online";
                      description = "The url for the api to use";
                    };
                  web-url =
                    mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "https://smos.online";
                      description = "The url that this web server is served from.";
                    };
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "Debug";
                      default = "Warn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      default = [ ];
                      example = [ "smos.online" ];
                      description = "The host to serve web requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8002;
                      description = "The port to serve web requests on";
                    };
                  google-analytics-tracking =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XX-XXXXXXXX-XX";
                      default = null;
                      description = "The Google analytics tracking code";
                    };
                  google-search-console-verification =
                    mkOption {
                      type = types.nullOr types.str;
                      example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                      default = null;
                      description = "The Google search console verification code";
                    };
                  pkg =
                    mkOption {
                      default = smosPackages.smos-web-server;
                      description = "The web server package";
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
        cfg.docs-site.config
        (attrOrNull "port" port)
        (attrOrNull "api-url" api-url)
        (attrOrNull "web-url" (if builtins.isNull web-url then head hosts else web-url))
        (attrOrNull "google-analytics-tracking" google-analytics-tracking)
        (attrOrNull "google-search-console-verification" google-search-console-verification)
      ];
      docs-site-service =
        optionalAttrs (cfg.docs-site.enable or false) {
          "smos-docs-site-${envname}" =
            with cfg.docs-site;
            {
              description = "Smos docs site ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment =
                {
                  "SMOS_DOCS_SITE_CONFIG_FILE" = "${toYamlFile "smos-docs-site-config" docs-site-config}";
                };
              script =
                ''
                  ${pkg}/bin/smos-docs-site serve
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
        optionalAttrs ((cfg.docs-site.enable or false) && hosts != [ ]) {
          "${head hosts}" =
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
      api-server-config = with cfg.api-server; mergeListRecursively [
        cfg.api-server.config
        (attrOrNull "log-level" log-level)
        (attrOrNull "port" port)
        (attrOrNull "database-file" api-server-database-file)
        (attrOrNull "admin" admin)
        (attrOrNull "max-backups-per-user" max-backups-per-user)
        (attrOrNull "max-backup-size-per-user" max-backup-size-per-user)
        (attrOrNull "auto-backup" auto-backup)
        (attrOrNull "backup-garbage-collector" backup-garbage-collector)
      ];
      # The api server
      api-server-service =
        optionalAttrs (cfg.api-server.enable or false) {
          "smos-api-server-${envname}" =
            with cfg.api-server;
            {
              description = "Smos API Server ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment =
                {
                  "SMOS_SERVER_CONFIG_FILE" = "${toYamlFile "smos-server-config" api-server-config}";
                  "SMOS_SERVER_DATABASE_FILE" = api-server-database-file;
                };
              script =
                ''
                  mkdir -p "${api-server-working-dir}"
                  cd ${api-server-working-dir}
                  ${pkg}/bin/smos-server \
                    serve
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
      api-server-host =
        optionalAttrs ((cfg.api-server.enable or false) && hosts != [ ]) {
          "${head hosts}" =
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
      web-server-config = with cfg.web-server; mergeListRecursively [
        cfg.web-server.config
        (attrOrNull "docs-url" docs-url)
        (attrOrNull "api-url" api-url)
        (attrOrNull "web-url" web-url)
        (attrOrNull "log-level" log-level)
        (attrOrNull "port" port)
        (attrOrNull "google-analytics-tracking" google-analytics-tracking)
        (attrOrNull "google-search-console-verification" google-search-console-verification)
        (attrOrNull "data-dir" web-server-data-dir)
      ];
      web-server-service =
        optionalAttrs (cfg.web-server.enable or false) {
          "smos-web-server-${envname}" =
            with cfg.web-server;
            {
              description = "Smos web server ${envname} Service";
              wantedBy = [ "multi-user.target" ];
              environment =
                {
                  "SMOS_WEB_SERVER_CONFIG_FILE" = "${toYamlFile "smos-web-server-config" web-server-config}";
                  "TERM" = "xterm-256color";
                };
              script =
                ''
                  mkdir -p "${web-server-working-dir}"
                  cd ${web-server-working-dir};
                  ${pkg}/bin/smos-web-server \
                    serve
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
      web-server-host =
        optionalAttrs (cfg.web-server.enable or false) {
          "${head hosts}" =
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
      systemd.services =
        mergeListRecursively [
          docs-site-service
          api-server-service
          web-server-service
          local-backup-service
        ];
      systemd.timers =
        mergeListRecursively [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional (cfg.docs-site.enable or false) cfg.docs-site.port)
        (optional (cfg.api-server.enable or false) cfg.api-server.port)
        (optional (cfg.web-server.enable or false) cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          docs-site-host
          api-server-host
          web-server-host
        ];
    };
}
