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
                      example = "smos.cs-syd.eu";
                      description = "The host to serve the docs site on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8000;
                      description = "The port to serve sync requests on";
                    };
                };
            };
        };
      sync-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos Sync Server";
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
                      example = "api.smos.cs-syd.eu";
                      description = "The host to serve sync requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve sync requests on";
                    };
                };
            };
        };
    };
  config =
    let
      smosPkgs = (import ../nix/pkgs.nix).smosPackages;
      docs-site-service =
        with cfg.docs-site;
        optionalAttrs enable {
          "smos-docs-${envname}" = {
            description = "Smos docs site ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SMOS_DOCS_SITE_PORT" =
                  "${builtins.toString port}";
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
      sync-server-service =
        with cfg.sync-server;

        let
          workingDir = "/www/smos/${envname}/data/";
        in
          optionalAttrs enable {
            "smos-sync-${envname}" = {
              description = "Smos sync server ${envname} Service";
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
      sync-server-host =
        with cfg.sync-server;

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
    in
      mkIf cfg.enable {
        systemd.services =
          concatAttrs [
            docs-site-service
            sync-server-service
          ];
        networking.firewall.allowedTCPPorts = builtins.concatLists [
          (optional cfg.docs-site.enable cfg.docs-site.port)
          (optional cfg.sync-server.enable cfg.sync-server.port)
        ];
        services.nginx.virtualHosts =
          concatAttrs [
            docs-site-host
            sync-server-host
          ];
      };
}
