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
  colour = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        background = lib.mkOption {
          default = null;
          description = "Table background colours";
          type = lib.types.nullOr (lib.types.oneOf [
            lib.types.anything
            (lib.types.submodule {
              options = {
                even = lib.mkOption {
                  default = null;
                  description = "background for even-numbered table-rows (0-indexed)";
                  type = lib.types.nullOr lib.types.anything;
                };
                odd = lib.mkOption {
                  default = null;
                  description = "background for odd-numbered table-rows";
                  type = lib.types.nullOr lib.types.anything;
                };
              };
            })
          ]);
        };
      };
    };
  };
  columns = lib.mkOption {
    default = null;
    description = "The columns in the report";
    type = lib.types.nullOr (lib.types.listOf lib.types.anything);
  };
  hide-archive = lib.mkOption {
    default = null;
    description = "Whether to consider archived entries";
    type = lib.types.nullOr lib.types.bool;
  };
  projects-dir = lib.mkOption {
    default = null;
    description = "The projects directory";
    type = lib.types.nullOr lib.types.str;
  };
  reports = lib.mkOption {
    default = null;
    description = "available reports";
    type = lib.types.nullOr (lib.types.attrsOf (lib.types.submodule {
      options = {
        columns = lib.mkOption {
          default = null;
          description = "The columns of the report";
          type = lib.types.nullOr (lib.types.listOf lib.types.anything);
        };
        description = lib.mkOption {
          default = null;
          description = "A description of the report";
          type = lib.types.nullOr lib.types.str;
        };
        filter = lib.mkOption {
          default = null;
          description = "The entry filter to get the results in the report";
          type = lib.types.nullOr lib.types.anything;
        };
        hide-archive = lib.mkOption {
          default = null;
          description = "Whether to consider the archive for the report";
          type = lib.types.nullOr lib.types.bool;
        };
        sorter = lib.mkOption {
          default = null;
          description = "The sorter to sort the rows of the report by";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    }));
  };
  work = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        base-filter = lib.mkOption {
          default = null;
          description = "The base work filter";
          type = lib.types.nullOr lib.types.anything;
        };
        checks = lib.mkOption {
          default = null;
          description = "Checks for the work report";
          type = lib.types.nullOr (lib.types.listOf lib.types.anything);
        };
        columns = lib.mkOption {
          default = null;
          description = "The columns in the report";
          type = lib.types.nullOr (lib.types.listOf lib.types.anything);
        };
        context = lib.mkOption {
          default = null;
          description = "The context that you are in";
          type = lib.types.nullOr lib.types.str;
        };
        contexts = lib.mkOption {
          default = null;
          description = "Contexts for the work report";
          type = lib.types.nullOr (lib.types.attrsOf lib.types.anything);
        };
        hide-archive = lib.mkOption {
          default = null;
          description = "Whether to consider archived entries";
          type = lib.types.nullOr lib.types.bool;
        };
        time = lib.mkOption {
          default = null;
          description = "A filter to filter by time";
          type = lib.types.nullOr lib.types.anything;
        };
        time-filter = lib.mkOption {
          default = null;
          description = "The property to use to filter by time";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    };
  };
  workflow-dir = lib.mkOption {
    default = null;
    description = "The workflow directory";
    type = lib.types.nullOr lib.types.str;
  };
}
