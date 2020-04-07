{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.smos."${envname}";
  concatAttrs = attrList: fold ( x: y: x // y ) {} attrList;
in {
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
                  host =
                    mkOption {
                      type = types.str;
                      example = "smos.cs-syd.eu";
                      description = "The host to serve the docs site on";
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
                      type = types.listOf ( types.str );
                      example = "api.smos.cs-syd.eu";
                      description = "The host to serve sync requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      default = 8000;
                      example = 8000;
                      description = "The port to serve sync requests on";
                    };
                };
            };
        };
    };
  config =
    let
      smosPkgs = import ../nix/pkgs.nix;
      smos-sync-service =
        let
          workingDir = "/www/smos/${envname}/data/";
          smos-server-static =
            pkgs.haskell.lib.justStaticExecutables ( smosPkgs.smosPackages.smos-server );
        in {
          description = "SmosSync ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          environment =
            {
              "SMOS_SERVER_LOG_LEVEL" =
                "${builtins.toString cfg.sync-server.log-level}";
              "SMOS_SERVER_PORT" =
                "${builtins.toString cfg.sync-server.port}";
            };
          script =
            ''
                mkdir -p "${workingDir}"
                cd "${workingDir}"
                ${smos-server-static}/bin/smos-server \
                  serve
              '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
        };
      docs-site-host =
        with cfg.docs-site;

        optionalAttrs enable {
          "${host}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" =
                {
                  root = "${smosPkgs.smosDocumentationSite}";
                  extraConfig =
                    ''
                      index index.html;
                      try_files $uri $uri/ $uri/index.html =404;
                    '';
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
              locations."/".proxyPass =
                "http://localhost:${builtins.toString port}";
              serverAliases = tail hosts;
            };
        };
    in
      mkIf cfg.enable {
        systemd.services =
          { "smos-sync-${envname}" = smos-sync-service; };
        networking.firewall.allowedTCPPorts =
          optional cfg.sync-server.enable cfg.sync-server.port;
        services.nginx.virtualHosts =
          optionalAttrs cfg.enable (
            concatAttrs [
              ( docs-site-host )
              ( sync-server-host )
            ]
          );
      };
}
