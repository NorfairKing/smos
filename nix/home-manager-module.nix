{ smosReleasePackages }:
{ lib
, pkgs
, config
, ...
}:

with lib;
let
  cfg = config.programs.smos;

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

in
{
  options.programs.smos = {
    enable = mkEnableOption "Smos";
    smosReleasePackages = mkOption {
      description = "The smosPackages attribute defined in the nix/overlay.nix file in the smos repository.";
      type = types.attrs;
      default = smosReleasePackages;
    };
    config = mkOption {
      default = { };
      description = "Typed contents of the config file";
      type = types.submodule {
        options = mergeListRecursively [
          (import ../smos/options.nix { inherit lib; })
          (import ../smos-archive/options.nix { inherit lib; })
          (import ../smos-calendar-import/options.nix { inherit lib; })
          (import ../smos-github/options.nix { inherit lib; })
          (import ../smos-jobhunt/options.nix { inherit lib; })
          (import ../smos-notify/options.nix { inherit lib; })
          (import ../smos-query/options.nix { inherit lib; })
          (import ../smos-scheduler/options.nix { inherit lib; })
          (import ../smos-sync-client-gen/options.nix { inherit lib; })
        ];
      };
    };
    extraConfig = mkOption {
      description = "Extra contents of the config file";
      default = { };
    };
    workflowDir = mkOption {
      description = "Smos' workflow directory";
      type = types.str;
      default = config.home.homeDirectory + "/workflow";
    };
    backup = mkOption {
      description = "Periodic local backups of the workflow directory";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Automatic Smos workflow backups";
          backupDir = mkOption {
            type = types.str;
            default = "${config.xdg.dataHome}/smos/backup";
            description = "The directory to backup to";
          };
          OnCalendar = mkOption {
            type = types.str;
            default = "daily";
            example = "weekly";
            description = "How frequently to run the local backup";
          };
        };
      });
      default = null;
    };
    sync = mkOption {
      description = "Periodic local backups of the workflow directory";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Automatic smos workflow synchronisation";
          OnCalendar = mkOption {
            type = types.str;
            default = "hourly";
            example = "daily";
            description = "How frequently to run the synchronisation";
          };
        };
      });
      default = null;
    };
    calendar = mkOption {
      description = "Periodic calendar imports";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Automatic smos calendar importing";
          OnCalendar = mkOption {
            type = types.str;
            default = "hourly";
            example = "daily";
            description = "How frequently to run the calendar import";
          };
        };
      });
      default = null;
    };
    scheduler = mkOption {
      description = "Automatic scheduled project scheduling";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Automatic smos scheduler activation";
          OnCalendar = mkOption {
            type = types.str;
            default = "hourly";
            example = "daily";
            description = "How frequently to run the scheduler";
          };
        };
      });
      default = null;
    };
    notify = mkOption {
      description = "Desktop notifications";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Automatic smos desktop notifications";
          notify-send = mkOption {
            type = types.package;
            default = pkgs.libnotify;
            description = "The package containing notify-send";
          };
          OnCalendar = mkOption {
            type = types.str;
            default = "minutely";
            example = "*:0/2";
            description = "How frequently to run the notifier";
          };
        };
      });
      default = null;
    };
    github = mkOption {
      description = "Smos Github";
      type = types.nullOr (types.submodule {
        options = {
          enable = mkEnableOption "Enable smos-github";
        };
      });
      default = null;
    };
  };
  config =
    let

      smosConfig = mergeListRecursively [
        cfg.config
        { workflow-dir = cfg.workflowDir; }
        cfg.extraConfig
      ];

      # Convert the config file to pretty yaml, for readability.
      # The keys will not be in the "right" order but that's fine.
      smosConfigFile = (pkgs.formats.yaml { }).generate "smos-config.yaml" smosConfig;

      makeConfigCheckScript = name: contents: "${pkgs.writeShellScript name ''
        ${contents}
        if [[ "$?" != "0" ]]
        then
          printf "${name} failed. This probably means you have an un-parseable configuration file. See above.\n" >&2
          exit 1
        fi
      ''}";
      queryConfigCheck = lib.hm.dag.entryAfter [ ] (makeConfigCheckScript "smos-query-config-check" ''
        $DRY_RUN_CMD ${cfg.smosReleasePackages.smos-query}/bin/smos-query --config-file=${smosConfigFile} next
      '');

      backupSmosName = "smos-backup";
      backupScript = pkgs.writeShellScript "${backupSmosName}-service-ExecStart" ''
        export PATH="$PATH:${pkgs.coreutils}/bin:${pkgs.gnutar}/bin:${pkgs.gzip}/bin"
        set -ex
        mkdir -p "${smosConfig.workflow-dir}"
        backupdir="${cfg.backup.backupDir}"
        mkdir -p "''${backupdir}"

        # We name the backup file based on the current date and time.
        # There are no spaces or colons in the filename so that it will be
        # easier to interact with the backup files in a shell script.
        backupfile="''${backupdir}/''$(date +%F_%H%M%S).tar.gz"

        # If the backup file already exists, then another backup was made
        # in the last second.
        # We then just don't make another.
        if [[ ! -f "''${backupfile}" ]]
        then
          tar -cvzf "''${backupfile}" "${smosConfig.workflow-dir}"
        fi
      '';
      backupSmosService = {
        Unit = {
          Description = "Backup smos locally, to ${cfg.backup.backupDir}";
        };
        Service = {
          ExecStart = "${backupScript}";
          Type = "oneshot";
        };
      };
      backupSmosTimer = {
        Unit = {
          Description = "Backup smos locally every day";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          inherit (cfg.backup) OnCalendar;
          Persistent = true;
          Unit = "${backupSmosName}.service";
        };
      };
      backupExtraActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        # Activate the backup during activaton, just in case something mucks up
        # the whole workflow in a new version.
        $DRY_RUN_CMD ${backupScript}
      '';

      syncSmosName = "smos-sync";
      syncSmosService = {
        Unit = {
          Description = "Sync smos workflow";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "${syncSmosName}-service-ExecStart" ''
              exec ${cfg.smosReleasePackages.smos-sync-client}/bin/smos-sync-client sync
            ''}";
          Type = "oneshot";
        };
      };
      syncSmosTimer = {
        Unit = {
          Description = "Sync smos every five minutes";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          inherit (cfg.sync) OnCalendar;
          Persistent = true;
          Unit = "${syncSmosName}.service";
        };
      };

      calendarSmosName = "smos-calendar-import";
      calendarSmosService = {
        Unit = {
          Description = "Import calendars into smos";
          Wants = [ "network-online.target" ];
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "${calendarSmosName}-service-ExecStart" ''
              exec ${cfg.smosReleasePackages.smos-calendar-import}/bin/smos-calendar-import
            ''}";
          Type = "oneshot";
        };
      };
      calendarSmosTimer = {
        Unit = {
          Description = "Import calendar into smos every day";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          inherit (cfg.calendar) OnCalendar;
          Persistent = true;
          Unit = "${calendarSmosName}.service";
        };
      };

      schedulerSmosName = "smos-scheduler";
      schedulerSmosService = {
        Unit = {
          Description = "smos-scheduler activation";
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "${schedulerSmosName}-service-ExecStart" ''
              set -e
              ${cfg.smosReleasePackages.smos-scheduler}/bin/smos-scheduler check
              exec ${cfg.smosReleasePackages.smos-scheduler}/bin/smos-scheduler schedule
            ''}";
          Type = "oneshot";
        };
      };
      schedulerSmosTimer = {
        Unit = {
          Description = "Activate smos scheduler every day";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          inherit (cfg.scheduler) OnCalendar;
          Persistent = true;
          Unit = "${schedulerSmosName}.service";
        };
      };
      schedulerConfigCheck = lib.hm.dag.entryAfter [ ] (makeConfigCheckScript "smos-scheduler-config-check" ''
        $DRY_RUN_CMD ${cfg.smosReleasePackages.smos-scheduler}/bin/smos-scheduler --config-file=${smosConfigFile} check
      '');

      notifySmosName = "smos-notify";
      notifySmosService = {
        Unit = {
          Description = "smos-notify activation";
        };
        Service = {
          ExecStart = "${pkgs.writeShellScript "${notifySmosName}-service-ExecStart" ''
              set -e
              export PATH="$PATH:${cfg.notify.notify-send}/bin:${pkgs.sox}/bin"
              exec ${cfg.smosReleasePackages.smos-notify}/bin/smos-notify
            ''}";
          Type = "oneshot";
        };
      };
      notifySmosTimer = {
        Unit = {
          Description = "Activate smos notify every minute";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
        Timer = {
          inherit (cfg.notify) OnCalendar;
          Persistent = true;
          Unit = "${notifySmosName}.service";
        };
      };
      activations = mergeListRecursively [
        # Checks
        { "smos-query-check" = queryConfigCheck; }
        (optionalAttrs (cfg.scheduler.enable or false) { "${schedulerSmosName}-check" = schedulerConfigCheck; })
        # Extra activation
        (optionalAttrs (cfg.backup.enable or false) { "${backupSmosName}-extra" = backupExtraActivation; })
      ];
      services = mergeListRecursively [
        (optionalAttrs (cfg.sync.enable or false) { "${syncSmosName}" = syncSmosService; })
        (optionalAttrs (cfg.backup.enable or false) { "${backupSmosName}" = backupSmosService; })
        (optionalAttrs (cfg.calendar.enable or false) { "${calendarSmosName}" = calendarSmosService; })
        (optionalAttrs (cfg.scheduler.enable or false) { "${schedulerSmosName}" = schedulerSmosService; })
        (optionalAttrs (cfg.notify.enable or false) { "${notifySmosName}" = notifySmosService; })
      ];
      timers = mergeListRecursively [
        (optionalAttrs (cfg.sync.enable or false) { "${syncSmosName}" = syncSmosTimer; })
        (optionalAttrs (cfg.backup.enable or false) { "${backupSmosName}" = backupSmosTimer; })
        (optionalAttrs (cfg.calendar.enable or false) { "${calendarSmosName}" = calendarSmosTimer; })
        (optionalAttrs (cfg.scheduler.enable or false) { "${schedulerSmosName}" = schedulerSmosTimer; })
        (optionalAttrs (cfg.notify.enable or false) { "${notifySmosName}" = notifySmosTimer; })
      ];
      packages = with cfg.smosReleasePackages;        [
        smos
        smos-archive
        smos-jobhunt
        smos-query
        smos-single
      ]
      ++ optional (cfg.sync.enable or false) smos-sync-client
      ++ optional (cfg.calendar.enable or false) smos-calendar-import
      ++ optional (cfg.scheduler.enable or false) smos-scheduler
      ++ optionals (cfg.notify.enable or false) [ smos-notify cfg.notify.notify-send ]
      ++ optional (cfg.github.enable or false) smos-github;

    in
    mkIf (cfg.enable or false) {
      xdg = {
        configFile."smos/config.yaml".source = smosConfigFile;
        mimeApps = {
          defaultApplications = {
            "text/smos" = [ "smos.desktop" ];
            "application/smos" = [ "smos.desktop" ];
          };
        };
      };
      systemd.user =
        {
          inherit services;
          inherit timers;
        };
      home.packages = packages;
      home.activation = activations;
    };
}
