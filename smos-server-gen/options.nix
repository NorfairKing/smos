{ lib }:
{
  admin = lib.mkOption {
    default = null;
    description = "The user that will have admin rights";
    type = lib.types.nullOr lib.types.str;
  };
  auto-backup = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        enable = lib.mkOption {
          default = null;
          description = "enable the auto-backup looper";
          type = lib.types.nullOr lib.types.bool;
        };
        period = lib.mkOption {
          default = null;
          description = "period of the auto-backup looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
        phase = lib.mkOption {
          default = null;
          description = "phase of the auto-backup looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
      };
    };
  };
  backup-garbage-collector = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        enable = lib.mkOption {
          default = null;
          description = "enable the backup-garbage-collector looper";
          type = lib.types.nullOr lib.types.bool;
        };
        period = lib.mkOption {
          default = null;
          description = "period of the backup-garbage-collector looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
        phase = lib.mkOption {
          default = null;
          description = "phase of the backup-garbage-collector looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
      };
    };
  };
  booking-email-address = lib.mkOption {
    default = null;
    description = "Email address to send booking emails from";
    type = lib.types.nullOr lib.types.str;
  };
  database-file = lib.mkOption {
    default = null;
    description = "The file to store the server database in";
    type = lib.types.nullOr lib.types.str;
  };
  file-migrator = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        enable = lib.mkOption {
          default = null;
          description = "enable the file-migrator looper";
          type = lib.types.nullOr lib.types.bool;
        };
        period = lib.mkOption {
          default = null;
          description = "period of the file-migrator looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
        phase = lib.mkOption {
          default = null;
          description = "phase of the file-migrator looper in seconds";
          type = lib.types.nullOr lib.types.number;
        };
      };
    };
  };
  log-level = lib.mkOption {
    default = null;
    description = "Minimal severity of log messages";
    type = lib.types.nullOr lib.types.anything;
  };
  max-backup-size-per-user = lib.mkOption {
    default = null;
    description = "The maximum number of bytes that backups can take up per user";
    type = lib.types.nullOr lib.types.ints.unsigned;
  };
  max-backups-per-user-per-period = lib.mkOption {
    default = null;
    description = "The maximum number of bytes that backups can take up per user";
    type = lib.types.nullOr (lib.types.oneOf [
      (lib.types.submodule {
        options = {
          max-backups = lib.mkOption {
            description = "maximum backups in this period";
            type = lib.types.ints.unsigned;
          };
          period = lib.mkOption {
            description = "period, in seconds";
            type = lib.types.number;
          };
        };
      })
      (lib.types.listOf (lib.types.submodule {
        options = {
          max-backups = lib.mkOption {
            description = "maximum backups in this period";
            type = lib.types.ints.unsigned;
          };
          period = lib.mkOption {
            description = "period, in seconds";
            type = lib.types.number;
          };
        };
      }))
    ]);
  };
  monetisation = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        freeloaders = lib.mkOption {
          default = null;
          description = "The usernames of users that will not have to pay, comma separated";
          type = lib.types.nullOr (lib.types.listOf lib.types.str);
        };
        stripe-price = lib.mkOption {
          default = null;
          description = "The stripe price id";
          type = lib.types.nullOr lib.types.str;
        };
        stripe-publishable-key = lib.mkOption {
          default = null;
          description = "The stripe api publishable key";
          type = lib.types.nullOr lib.types.str;
        };
        stripe-secret-key = lib.mkOption {
          default = null;
          description = "The stripe api secret key";
          type = lib.types.nullOr lib.types.str;
        };
        stripe-secret-key-file = lib.mkOption {
          default = null;
          description = "The stripe api secret key";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  port = lib.mkOption {
    default = null;
    description = "The port to serve web requests on";
    type = lib.types.nullOr lib.types.int;
  };
  signing-key-file = lib.mkOption {
    default = null;
    description = "The file to store the JWT signing key in";
    type = lib.types.nullOr lib.types.str;
  };
  uuid-file = lib.mkOption {
    default = null;
    description = "The file to store the server uuid in";
    type = lib.types.nullOr lib.types.str;
  };
}
