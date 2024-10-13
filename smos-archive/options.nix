{ lib }:
{
  archive = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        log-level = lib.mkOption {
          default = null;
          description = "Minimal severity of log messages";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    };
  };
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
  directory = lib.mkOption {
    default = null;
    description = "The directory to export the archive to";
    type = lib.types.nullOr lib.types.str;
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
