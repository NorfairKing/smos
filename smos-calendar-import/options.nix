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
  calendar = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        log-level = lib.mkOption {
          default = null;
          description = "Minimal severity of log messages";
          type = lib.types.nullOr lib.types.anything;
        };
        sources = lib.mkOption {
          default = null;
          description = "Calendar sources to import from";
          type = lib.types.nullOr (lib.types.listOf (lib.types.submodule {
            options = {
              destination = lib.mkOption {
                description = "The destination path within the workflow directory";
                type = lib.types.str;
              };
              name = lib.mkOption {
                default = null;
                description = "The name of the source";
                type = lib.types.nullOr lib.types.str;
              };
              source = lib.mkOption {
                default = null;
                description = "the url to fetch or file to import";
                type = lib.types.nullOr lib.types.str;
              };
              source-file = lib.mkOption {
                default = null;
                description = "the file that contains the url to fetch or file to import";
                type = lib.types.nullOr lib.types.str;
              };
            };
          }));
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
