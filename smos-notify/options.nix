{ lib }:
{
  archive-dir = lib.mkOption {
    default = null;
    description = "The archive directory";
    type = lib.types.nullOr lib.types.str;
  };
  archived-projects-dir = lib.mkOption {
    default = null;
    description = "The archived projects directory";
    type = lib.types.nullOr lib.types.str;
  };
  notify = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        database = lib.mkOption {
          default = null;
          description = "Path to database file";
          type = lib.types.nullOr lib.types.str;
        };
        log-level = lib.mkOption {
          default = null;
          description = "Minimal severity of log messages";
          type = lib.types.nullOr lib.types.anything;
        };
        notify-send = lib.mkOption {
          default = null;
          description = "Path to notify-send executable";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  projects-dir = lib.mkOption {
    default = null;
    description = "The projects directory";
    type = lib.types.nullOr lib.types.str;
  };
  workflow-dir = lib.mkOption {
    default = null;
    description = "The workflow directory";
    type = lib.types.nullOr lib.types.str;
  };
}
