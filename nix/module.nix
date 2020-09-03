{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.smos."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) {} attrList;
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
                      example = "docs.smos.online";
                      description = "The host to serve the docs site on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8000;
                      description = "The port to serve sync requests on";
                    };
                  web-url =
                    mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      example = "https://smos.online";
                      description = "The url for the web server to refer to";
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
                };
            };
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
                      example = "smos.online";
                      description = "The host to serve web requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8002;
                      description = "The port to serve web requests on";
                    };
                };
            };
        };
    };
  config =
    let
      smosPkgs = (import ../nix/pkgs.nix).smosPackages;
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
              } // optionalAttrs (!(builtins.isNull web-url)) {
                "SMOS_DOCS_SITE_WEB_SERVER_URL" = "${web-url}";
              };
            script =
              ''
                ${smosPkgs.smos-docs-site}/bin/smos-docs-site
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

        optionalAttrs enable {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/".proxyPass =
                "http://localhost:${builtins.toString port}";
              serverAliases = tail hosts;
            };
        };
      # The api server
      api-server-service =
        with cfg.api-server;

        let
          workingDir = "/www/smos/${envname}/api-server/";
        in
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
                };
              script =
                ''
                  mkdir -p "${workingDir}"
                  cd "${workingDir}"
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

        optionalAttrs enable {
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
      # The web server
      web-server-service =
        with cfg.web-server;

        let
          workingDir = "/www/smos/${envname}/web-server/";
        in
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
                  "SMOS_WEB_SERVER_DATA_DIR" = workingDir + "workflows/";
                  "TERM" = "xterm-256color";
                };
              script =
                ''
                  mkdir -p "${workingDir}"
                  cd "${workingDir}"
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
