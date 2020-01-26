{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.smos;


in {
  options =
    {
      programs.smos =
        {
          enable = mkEnableOption "Smos cli and syncing";
          workflowDir =
            mkOption {
              type = types.str;
              description = "Smos' workflow directory";
              default = config.home.homeDirectory + "/workflow";
            };
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          backup =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Smos backups";
                        backupDir =
                          mkOption {
                            type = types.str;
                            default = ".smos/backup";
                            description = "The directory to backup to";
                          };
                      };
                  }
                );
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Smos syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.smos.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      smosPkgs = (import ./pkgs.nix).smosPackages;

      syncConfigContents =
        syncCfg:
          optionalString ( syncCfg.enable or false ) ''

sync:
  server-url: "${cfg.sync.server-url}"
  username: "${cfg.sync.username}"
  password: "${cfg.sync.password}"

    '';

      smosConfigContents =
        concatStringsSep "\n" [
          ( syncConfigContents cfg.sync )
          cfg.extraConfig
        ];

      backupSmosName = "backup-smos";
      backupSmosService =
        {
          Unit =
            {
              Description = "Backup smos";
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "backup-smos-service-ExecStart"
            ''
              export PATH="$PATH:${pkgs.coreutils}/bin"
              set -ex
              backupdir="$HOME/${cfg.backup.backupDir}"
              subdir="$backupdir/''$(date +%F_%T)/"
              mkdir -p "$subdir"
              exec ${pkgs.rsync}/bin/rsync --recursive ${cfg.workflowDir}/ "$subdir"
            ''}";
              Type = "oneshot";
            };
        };
      backupSmosTimer =
        {
          Unit =
            {
              Description = "Backup smos every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*-*-* 00:00";
              Persistent = true;
              Unit = "${backupSmosName}.service";
            };
        };

      syncSmosName = "sync-smos";
      syncSmosService =
        {
          Unit =
            {
              Description = "Sync smos";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-smos-service-ExecStart" 
            ''
              exec ${smosPkgs.smos-sync-client}/bin/smos-sync-client sync
            ''}";
              Type = "oneshot";
            };
        };
      syncSmosTimer =
        {
          Unit =
            {
              Description = "Sync smos every five minutes for";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*:0/5";
              Persistent = true;
              Unit = "${syncSmosName}.service";
            };
        };

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
          "${syncSmosName}" = syncSmosService;
        }
        //
        optionalAttrs (cfg.backup.enable or false) {
          "${backupSmosName}" = backupSmosService;
        }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
          "${syncSmosName}" = syncSmosTimer;
        }
        //
        optionalAttrs (cfg.backup.enable or false) {
          "${backupSmosName}" = backupSmosTimer;
        }
        );
      packages =
        [
          smosPkgs.smos
          smosPkgs.smos-archive
          smosPkgs.smos-convert-org
          smosPkgs.smos-single
          smosPkgs.smos-query
          smosPkgs.smos-sync-client
        ];


    in
      mkIf cfg.enable {
        xdg.configFile."smos/config.yaml".text = smosConfigContents;
        systemd.user =
          {
            startServices = true;
            services = services;
            timers = timers;
          };
        home.packages = packages;
      };
}
