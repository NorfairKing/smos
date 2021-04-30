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
                  tests = mkOption {
                    default = [ ];
                    type =
                      types.attrsOf (types.submodule {
                        options = {

                          enable = mkEnableOption "Test of this environment";
                          api-url =
                            mkOption {
                              type = types.str;
                              example = "api.testing.smos.online";
                              description = "The url for the api to test";
                            };
                          time =
                            mkOption {
                              type = types.str;
                              example = "03:00";
                              description = "The time of day to start the end-to-end test.";
                            };
                        };
                      });
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
      end-to-end-api-server-test-services =
        with cfg.api-server;
        optionalAttrs enable (mergeListRecursively (mapAttrsToList
          (name: test:
            with test;
            optionalAttrs enable {
              "smos-api-server-end-to-end-test-${envname}-${name}" = {
                description = "Smos end to end test of ${envname} to ${name}";
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
            })
          tests));
      end-to-end-api-server-test-timers =
        with cfg.api-server;
        optionalAttrs enable (mergeListRecursively (mapAttrsToList
          (name: test:
            with test;
            optionalAttrs enable {
              "smos-api-server-end-to-end-test-${envname}-${name}" = {
                description = "Run the end-to-end tests from ${envname} to ${name} every day";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "*-*-* ${time}";
                };
              };
            })
          tests));
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          end-to-end-api-server-test-services
        ];
      systemd.timers =
        mergeListRecursively [
          end-to-end-api-server-test-timers
        ];
    };
}
