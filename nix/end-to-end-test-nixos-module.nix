{ envname }:
{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.smos."${envname}".end-to-end-testing;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.smos."${envname}".end-to-end-testing =
    {
      enable = mkEnableOption "Smos End to End Testing";
      api-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Smos API Server";
                  api-url =
                    mkOption {
                      type = types.str;
                      example = "api.testing.smos.online";
                      description = "The url for the api to test";
                    };
                };
            };
          default = null;
        };
    };
  config =
    let
      smosPkgs = (import ./pkgs.nix { }).smosPackages;
      working-dir = "/www/smos/end-to-end-testing/";
      end-to-end-api-server-test-service =
        with cfg.api-server;
        optionalAttrs enable {
          "smos-server-end-to-end-test" = {
            description = "Smos end to end test";
            environment =
              {
                "SMOS_SERVER_URL" = api-url;
              };
            script =
              ''
                ${smosPkgs.smos-server-gen}/bin/smos-server-end-to-end-test
              '';
            serviceConfig =
              {
                Type = "oneshot";
              };
          };
        };
      end-to-end-api-server-test-timer =
        with cfg.api-server;
        optionalAttrs enable {
          "smos-api-server-end-to-end-test" = {
            description = "Run the end-to-end tests every day";
            wantedBy = [ "timers.target" ];
            timerConfig = {
              OnCalendar = "daily";
              Persistent = true;
            };
          };
        };
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          end-to-end-api-server-test-service
        ];
      systemd.timers =
        mergeListRecursively [
          end-to-end-api-server-test-timer
        ];
    };
}
