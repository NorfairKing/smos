{ envname
, sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, smosReleasePackages ? pkgs.smosReleasePackages
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
      docs-site = mkOption {
        default = null;
        description = "Smos' documentation site service";
        type = types.nullOr (types.submodule {
          options =
            {
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
                default = smosReleasePackages.smos-docs-site;
              };
            };
        });
      };
      api-server = mkOption {
        default = null;
        description = "Smos' API server service";
        type = types.nullOr (types.submodule {
          options =
            {
              enable = mkEnableOption "Smos API Server";
              config = mkOption {
                description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                type = types.attrs;
                default = { };
              };
              port = mkOption {
                description = "The port to serve api requests on";
                type = types.int;
                example = 8001;
              };
              log-level = mkOption {
                description = "The log level to use";
                type = types.str;
                example = "Debug";
                default = "Warn";
              };
              hosts = mkOption {
                description = "The host to serve api requests on";
                type = types.listOf (types.str);
                example = "api.smos.online";
              };
              admin = mkOption {
                description = "The username of the admin user";
                type = types.nullOr types.str;
                example = "admin";
                default = null;
              };
              max-backups-per-user = mkOption {
                description = "The maximum number of backups per user";
                type = types.nullOr types.int;
                default = null;
                example = 5;
              };
              max-backup-size-per-user = mkOption {
                description = "The maximum number of bytes that backups can take up per user";
                type = types.nullOr types.int;
                default = null;
                example = 1024 * 1024;
              };
              backup-interval = mkOption {
                description = "The interval between automatic backups (seconds)";
                type = types.nullOr types.int;
                default = null;
                example = 3600;
              };
              local-backup = mkOption {
                description = "The local backup service for the API server database";
                type = types.nullOr (types.submodule {
                  options = {
                    enable = mkEnableOption "Smos API Server Local Backup Service";
                    backup-dir = mkOption {
                      type = types.str;
                      example = "backup/api-server";
                      default = "backup/api-server";
                      description = "The directory to store backups in, relative to the /www/smos/${envname} directory or absolute";
                    };
                  };
                });
                default = null;
              };
              auto-backup = mkLooperOption "auto-backup";
              backup-garbage-collector = mkLooperOption "backup-garbage-collector";
              file-migrator = mkLooperOption "file-migrator";
              pkg = mkOption {
                description = "The docs server package";
                type = types.package;
                default = smosReleasePackages.smos-server;
              };
              monetisation = mkOption {
                description = "Monetisation settings for the API server";
                type = types.nullOr (types.submodule {
                  options = {
                    stripe-secret-key = mkOption {
                      description = "The stripe api secret key";
                      type = types.str;
                      example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXX";
                    };
                    stripe-publishable-key = mkOption {
                      description = "The stripe api publishable key";
                      type = types.str;
                      example = "pk_test_XXXXXXXXXXXXXXXXXXXXXXX";
                    };
                    stripe-price = mkOption {
                      description = "The stripe price";
                      type = types.str;
                      example = "price_XXXXXXXXXXXXXXXXXXXXXXXX";
                    };
                    freeloaders = mkOption {
                      description = "The usernames of users that will not have to pay";
                      type = types.listOf types.str;
                      default = [ ];
                      example = [ "friend" ];
                    };
                  };
                });
                default = null;
              };
            };
        });
      };
      web-server = mkOption {
        default = null;
        description = "Smos' web server service";
        type = types.nullOr (types.submodule {
          options =
            {
              enable = mkEnableOption "Smos Web Server";
              config = mkOption {
                description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                type = types.attrs;
                default = { };
              };
              docs-url = mkOption {
                description = "The url for the docs to refer to";
                type = types.str;
                example = "docs.smos.online";
                default = "docs.smos.online";
              };
              api-url = mkOption {
                description = "The url for the api to use";
                type = types.str;
                example = "api.smos.online";
              };
              web-url = mkOption {
                description = "The url that this web server is served from.";
                type = types.nullOr types.str;
                default = null;
                example = "https://smos.online";
              };
              data-dir = mkOption {
                description = "The directory to store workflows during editing";
                type = types.nullOr types.str;
                default = null;
                example = "/www/smos/production/web-server/web-server/";
              };
              log-level = mkOption {
                description = "The log level to use";
                type = types.str;
                example = "Debug";
                default = "Warn";
              };
              hosts = mkOption {
                description = "The host to serve web requests on";
                type = types.listOf (types.str);
                default = [ ];
                example = [ "smos.online" ];
              };
              port = mkOption {
                description = "The port to serve web requests on";
                type = types.int;
                example = 8002;
              };
              google-analytics-tracking = mkOption {
                description = "The Google analytics tracking code";
                type = types.nullOr types.str;
                default = null;
                example = "XX-XXXXXXXX-XX";
              };
              google-search-console-verification = mkOption {
                description = "The Google search console verification code";
                type = types.nullOr types.str;
                default = null;
                example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
              };
              pkg = mkOption {
                description = "The web server package";
                type = types.package;
                default = smosReleasePackages.smos-web-server;
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
      api-server-config = with cfg.api-server; mergeListRecursively [
        (attrOrNull "log-level" log-level)
        (attrOrNull "port" port)
        (attrOrNull "database-file" api-server-database-file)
        (attrOrNull "admin" admin)
        (attrOrNull "max-backups-per-user" max-backups-per-user)
        (attrOrNull "max-backup-size-per-user" max-backup-size-per-user)
        (attrOrNull "backup-interval" backup-interval)
        (attrOrNull "auto-backup" auto-backup)
        (attrOrNull "backup-garbage-collector" backup-garbage-collector)
        (attrOrNull "file-migrator" file-migrator)
        (attrOrNull "monetisation" monetisation)
        cfg.api-server.config
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
                  ${pkg}/bin/smos-server
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
      web-server-config = with cfg.web-server; mergeListRecursively [
        (attrOrNull "docs-url" docs-url)
        (attrOrNull "api-url" api-url)
        (attrOrNull "web-url" web-url)
        (attrOrNull "log-level" log-level)
        (attrOrNull "port" port)
        (attrOrNull "google-analytics-tracking" google-analytics-tracking)
        (attrOrNull "google-search-console-verification" google-search-console-verification)
        (attrOrNull "data-dir" web-server-data-dir)
        cfg.web-server.config
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
                  ${pkg}/bin/smos-web-server
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
