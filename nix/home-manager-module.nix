{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.programs.smos;

  mergeListRecursively = attrList:
    fold
      (
        x: y:
          lib.recursiveUpdate x y
      )
      { }
      attrList;


in
{
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
          config =
            mkOption {
              # type = types.str;
              description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place.";
              default = { };
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
                            default = "${config.xdg.dataHome}/smos/backup";
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
          calendar =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Smos calendar importing";
                        sources =
                          mkOption {
                            description = "The list of sources to import from";
                            default = [ ];
                            type = types.listOf (
                              types.submodule {
                                options = {
                                  name =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "Personal";
                                      description = "The name of the source";
                                    };
                                  destination =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "calendar/name.smos";
                                      description = "The destination file within the workflow directory";
                                    };
                                  source =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "https://calendar.google.com/calendar/ical/xxx.xxxxxxxxx%40gmail.com/private-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/basic.ics";
                                      description = "The url to download the calendar from";
                                    };
                                };
                              }
                            );
                          };
                      };
                  }
                );
            };
          scheduler =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Smos scheduler activation";
                        schedule =
                          mkOption {
                            description = "The schedule to activate";
                            default = [ ];
                            type = types.listOf (
                              types.submodule {
                                options = {
                                  description =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "Weekly tasks for work";
                                      description = "A description of the schedule item. This is only used for logging and error messages.";
                                    };
                                  template =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "templates/weekly.smos";
                                      description = "The relative path to the template in the workflow dir";
                                    };
                                  destination =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "workflow/work-[ %Y-%V | monday ].smos";
                                      description = "The template relative path to the destination in the workflow dir";
                                    };
                                  schedule =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "0 12 * * 6"; # At 12:00 on saturday
                                      description = "The cron schedule for when to activate this item";
                                    };
                                };
                              }
                            );
                          };
                      };
                  }
                );
            };
          notify =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Smos notification activation";
                      };
                  }
                );
            };
        };
    };
  config =
    let
      smosPkgs = (import ./pkgs.nix { }).smosPackages;
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
                    export PATH="$PATH:${pkgs.coreutils}/bin:${pkgs.gnutar}/bin:${pkgs.gzip}/bin"
                    set -ex
                    backupdir="${cfg.backup.backupDir}"
                    mkdir -p "''${backupdir}"
                    backupfile="''${backupdir}/''$(date +%F_%T).tar.gz"
                    tar -cvzf "''${backupfile}" "${cfg.workflowDir}"
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

      syncConfig = optionalAttrs (cfg.sync.enable or false) {
        sync = {
          server-url = cfg.sync.server-url;
          username = cfg.sync.username;
          password = cfg.sync.password;
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

      calendarConfig =
        optionalAttrs (cfg.calendar.enable or false) {
          calendar = cfg.calendar;
        };


      calendarSmosName = "calendar-smos";
      calendarSmosService =
        {
          Unit =
            {
              Description = "Calendar import smos";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "calendar-smos-service-ExecStart"
                  ''
                    exec ${smosPkgs.smos-calendar-import}/bin/smos-calendar-import
                  ''}";
              Type = "oneshot";
            };
        };
      calendarSmosTimer =
        {
          Unit =
            {
              Description = "Import calendar into smos every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "hourly";
              Persistent = true;
              Unit = "${calendarSmosName}.service";
            };
        };

      schedulerConfig =
        optionalAttrs (cfg.scheduler.enable or false) {
          scheduler = cfg.scheduler;
        };

      schedulerSmosName = "scheduler-activate-smos";
      schedulerSmosService =
        {
          Unit =
            {
              Description = "smos-scheduler activation";
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "scheduler-activate-smos-service-ExecStart"
                  ''
                    set -e
                    ${smosPkgs.smos-scheduler}/bin/smos-scheduler check
                    exec ${smosPkgs.smos-scheduler}/bin/smos-scheduler schedule
                  ''}";
              Type = "oneshot";
            };
        };
      schedulerSmosTimer =
        {
          Unit =
            {
              Description = "Activate smos scheduler every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "hourly";
              Persistent = true;
              Unit = "${schedulerSmosName}.service";
            };
        };

      notifySmosName = "notify-smos";
      notifySmosService =
        {
          Unit =
            {
              Description = "smos-notify activation";
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "notify-smos-service-ExecStart"
                  ''
                    set -e
                    export PATH="$PATH:${pkgs.libnotify}/bin:${pkgs.sox}/bin"
                    exec ${smosPkgs.smos-notify}/bin/smos-notify
                  ''}";
              Type = "oneshot";
            };
        };
      notifySmosTimer =
        {
          Unit =
            {
              Description = "Activate smos notify every minute";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "minutely";
              Persistent = true;
              Unit = "${notifySmosName}.service";
            };
        };

      smosConfig = mergeListRecursively [
        syncConfig
        calendarConfig
        schedulerConfig
        cfg.config
      ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      smosConfigContents = pkgs.stdenv.mkDerivation {
        name = "smos-config-contents";
        buildInputs = [ pkgs.haskellPackages.json2yaml ];
        src = "${pkgs.writeText "smos-config.yaml" (builtins.toJSON smosConfig)}";
        buildCommand = ''
          json2yaml $src > $out
        '';
      };

      services =
        (
          optionalAttrs (cfg.sync.enable or false)
            {
              "${syncSmosName}" = syncSmosService;
            }
          // optionalAttrs (cfg.backup.enable or false) {
            "${backupSmosName}" = backupSmosService;
          }
          // optionalAttrs (cfg.calendar.enable or false) {
            "${calendarSmosName}" = calendarSmosService;
          }
          // optionalAttrs (cfg.scheduler.enable or false) {
            "${schedulerSmosName}" = schedulerSmosService;
          }
          // optionalAttrs (cfg.notify.enable or false) {
            "${notifySmosName}" = notifySmosService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false)
            {
              "${syncSmosName}" = syncSmosTimer;
            }
          // optionalAttrs (cfg.backup.enable or false) {
            "${backupSmosName}" = backupSmosTimer;
          }
          // optionalAttrs (cfg.calendar.enable or false) {
            "${calendarSmosName}" = calendarSmosTimer;
          }
          // optionalAttrs (cfg.scheduler.enable or false) {
            "${schedulerSmosName}" = schedulerSmosTimer;
          }
          // optionalAttrs (cfg.notify.enable or false) {
            "${notifySmosName}" = notifySmosTimer;
          }
        );
      packages =
        [
          smosPkgs.smos
          smosPkgs.smos-archive
          smosPkgs.smos-calendar-import
          smosPkgs.smos-convert-org
          smosPkgs.smos-query
          smosPkgs.smos-scheduler
          smosPkgs.smos-single
          smosPkgs.smos-sync-client
          smosPkgs.smos-github
        ] ++ optional (cfg.notify.enable or false) smosPkgs.smos-notify;


    in
    mkIf cfg.enable {
      xdg = {
        configFile."smos/config.yaml".source = "${smosConfigContents}";
        mimeApps = {
          defaultApplications = {
            "text/smos" = [ "smos.desktop" ];
            "application/smos" = [ "smos.desktop" ];
          };
        };
      };
      systemd.user =
        {
          startServices = true;
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
