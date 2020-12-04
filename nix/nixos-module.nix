{ envname }:
{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.smos."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) { } attrList;
in
{
  options.services.smos."${envname}" =
    {
      enable = mkEnableOption "Smos Service";
      docs-site =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos Docs Site";
                  hosts =
                    mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      example = [ "docs.smos.online" ];
                      description = "The host to serve the docs site on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8000;
                      description = "The port to serve sync requests on";
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
                };
            };
        };
      api-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos API Server";
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
                  port =
                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve api requests on";
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
                };
            };
          default = null;
        };
      web-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos Web Server";
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
                };
            };
        };
    };
  config =
    let
      smosPkgs = (import ./pkgs.nix { }).smosPackages;
      working-dir = "/www/smos/${envname}/";
      # The docs server
      docs-site-service =
        with cfg.docs-site;
        optionalAttrs enable {
          "smos-docs-site-${envname}" = {
            description = "Smos docs site ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SMOS_DOCS_SITE_PORT" = "${builtins.toString port}";
              } // optionalAttrs (!builtins.isNull api-url) {
                "SMOS_DOCS_SITE_API_URL" = "${api-url}";
              } // optionalAttrs (!builtins.isNull web-url) {
                "SMOS_DOCS_SITE_WEB_URL" = "${web-url}";
              } // optionalAttrs (!builtins.isNull google-analytics-tracking) {
                "SMOS_DOCS_SITE_GOOGLE_ANALYTICS_TRACKING" = "${google-analytics-tracking}";
              } // optionalAttrs (!builtins.isNull google-search-console-verification) {
                "SMOS_DOCS_SITE_GOOGLE_SEARCH_CONSOLE_VERIFICATION" = "${google-search-console-verification}";
              };
            script =
              ''
                ${smosPkgs.smos-docs-site}/bin/smos-docs-site serve
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
        with cfg.docs-site;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
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
      # The api server
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "smos-api-server-${envname}" = {
            description = "Smos API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SMOS_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "SMOS_SERVER_PORT" =
                  "${builtins.toString port}";
                "SMOS_SERVER_DATABASE_FILE" = api-server-database-file;
              };
            script =
              ''
                mkdir -p "${api-server-working-dir}"
                cd ${api-server-working-dir}
                ${smosPkgs.smos-server}/bin/smos-server \
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
        with cfg.api-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
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
      web-server-service =
        with cfg.web-server;
        optionalAttrs enable {
          "smos-web-server-${envname}" = {
            description = "Smos web server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SMOS_WEB_SERVER_API_URL" = "${api-url}";
                "SMOS_WEB_SERVER_DOCS_URL" = "${docs-url}";
                "SMOS_WEB_SERVER_LOG_LEVEL" = "${builtins.toString log-level}";
                "SMOS_WEB_SERVER_PORT" = "${builtins.toString port}";
                "SMOS_WEB_SERVER_DATA_DIR" = web-server-data-dir;
                "TERM" = "xterm-256color";
              } // optionalAttrs (!builtins.isNull google-analytics-tracking) {
                "SMOS_WEB_SERVER_GOOGLE_ANALYTICS_TRACKING" = "${google-analytics-tracking}";
              } // optionalAttrs (!builtins.isNull google-search-console-verification) {
                "SMOS_WEB_SERVER_GOOGLE_SEARCH_CONSOLE_VERIFICATION" = "${google-search-console-verification}";
              };
            script =
              ''
                mkdir -p "${web-server-working-dir}"
                cd ${web-server-working-dir};
                ${smosPkgs.smos-web-server}/bin/smos-web-server \
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
        with cfg.web-server;

        optionalAttrs enable {
          "${head hosts}" =
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
    mkIf cfg.enable {
      systemd.services =
        concatAttrs [
          docs-site-service
          api-server-service
          web-server-service
          local-backup-service
        ];
      systemd.timers =
        concatAttrs [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.docs-site.enable cfg.docs-site.port)
        (optional cfg.api-server.enable cfg.api-server.port)
        (optional cfg.web-server.enable cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        concatAttrs [
          docs-site-host
          api-server-host
          web-server-host
        ];
    };
}
