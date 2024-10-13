{ lib }:
{
  any = lib.mkOption {
    default = null;
    description = "Keybindings for any context";
    type = lib.types.nullOr lib.types.anything;
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
  browser = lib.mkOption {
    default = null;
    description = "Keybindings for the file browser context";
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        any = lib.mkOption {
          default = null;
          description = "Keybindings for any of the other file browser situations";
          type = lib.types.nullOr lib.types.anything;
        };
        empty = lib.mkOption {
          default = null;
          description = "Keybindings for when the directory being browsed is empty";
          type = lib.types.nullOr lib.types.anything;
        };
        existent = lib.mkOption {
          default = null;
          description = "Keybindings for when an existing file or directory is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        filter = lib.mkOption {
          default = null;
          description = "Keybindings for when file browser's filter bar is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        in-progress = lib.mkOption {
          default = null;
          description = "Keybindings for when an in-progress file or directory is selected";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    });
  };
  explainer-mode = lib.mkOption {
    default = null;
    description = "Activate sandbox mode to ensure that smos can only edit smos files";
    type = lib.types.nullOr lib.types.bool;
  };
  file = lib.mkOption {
    default = null;
    description = "Keybindings for the file context";
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        any = lib.mkOption {
          default = null;
          description = "Keybindings that match in any file subcontext";
          type = lib.types.nullOr lib.types.anything;
        };
        contents = lib.mkOption {
          default = null;
          description = "Keybindings for when an contents is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        empty = lib.mkOption {
          default = null;
          description = "Keybindings for when the file is empty";
          type = lib.types.nullOr lib.types.anything;
        };
        entry = lib.mkOption {
          default = null;
          description = "Keybindings for when an entry is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        header = lib.mkOption {
          default = null;
          description = "Keybindings for when an header is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        logbook = lib.mkOption {
          default = null;
          description = "Keybindings for when a logbook is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        properties = lib.mkOption {
          default = null;
          description = "Keybindings for when a properties are selected";
          type = lib.types.nullOr lib.types.anything;
        };
        state-history = lib.mkOption {
          default = null;
          description = "Keybindings for when a state history is selected";
          type = lib.types.nullOr lib.types.anything;
        };
        tags = lib.mkOption {
          default = null;
          description = "Keybindings for when a tags are selected";
          type = lib.types.nullOr lib.types.anything;
        };
        timestamps = lib.mkOption {
          default = null;
          description = "Keybindings for when a timestamps are selected";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    });
  };
  free = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        earliest = lib.mkOption {
          default = null;
          description = "the earliest time of day to consider free";
          type = lib.types.nullOr lib.types.str;
        };
        latest = lib.mkOption {
          default = null;
          description = "the latest time of day to consider free";
          type = lib.types.nullOr lib.types.str;
        };
      };
    };
  };
  help = lib.mkOption {
    default = null;
    description = "Keybindings for the help context";
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        any = lib.mkOption {
          default = null;
          description = "Keybindings for at any time in the help screen";
          type = lib.types.nullOr lib.types.anything;
        };
        help = lib.mkOption {
          default = null;
          description = "Keybindings for when in the help screen";
          type = lib.types.nullOr lib.types.anything;
        };
        search = lib.mkOption {
          default = null;
          description = "Keybindings for when the search bar is selected within the help screen";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    });
  };
  projects-dir = lib.mkOption {
    default = null;
    description = "The projects directory";
    type = lib.types.nullOr lib.types.str;
  };
  reports = lib.mkOption {
    default = null;
    description = "Keybindings for the reports context";
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        any = lib.mkOption {
          default = null;
          description = "Keybindings for at any point in any report";
          type = lib.types.nullOr lib.types.anything;
        };
        next-action = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive next action report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the next-action report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the next-action report";
                type = lib.types.nullOr lib.types.anything;
              };
              search = lib.mkOption {
                default = null;
                description = "Keybindings for the search in the next-action report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
        ongoing = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive ongoing report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the ongoing report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the ongoing report";
                type = lib.types.nullOr lib.types.anything;
              };
              search = lib.mkOption {
                default = null;
                description = "Keybindings for the search in the ongoing report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
        stuck = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive stuck projects report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the stuck report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the stuck report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
        timestamps = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive timestamps report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the timestamps report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the timestamps report";
                type = lib.types.nullOr lib.types.anything;
              };
              search = lib.mkOption {
                default = null;
                description = "Keybindings for the search in the timestamps report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
        waiting = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive waiting report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the waiting report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the waiting report";
                type = lib.types.nullOr lib.types.anything;
              };
              search = lib.mkOption {
                default = null;
                description = "Keybindings for the search in the waiting report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
        work = lib.mkOption {
          default = null;
          description = "Keybindings for the interactive work report";
          type = lib.types.nullOr (lib.types.submodule {
            options = {
              any = lib.mkOption {
                default = null;
                description = "Keybindings for at any point in the work report";
                type = lib.types.nullOr lib.types.anything;
              };
              normal = lib.mkOption {
                default = null;
                description = "Keybindings for interacting with the work report";
                type = lib.types.nullOr lib.types.anything;
              };
              search = lib.mkOption {
                default = null;
                description = "Keybindings for the search in the work report";
                type = lib.types.nullOr lib.types.anything;
              };
            };
          });
        };
      };
    });
  };
  reset = lib.mkOption {
    default = null;
    description = "Whether to reset all keybindings. Set this to false to add keys, set this to true to replace keys.";
    type = lib.types.nullOr lib.types.bool;
  };
  stuck = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        threshold = lib.mkOption {
          default = null;
          description = "stuck report threshold to consider stuck projects 'overdue'";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    };
  };
  waiting = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        threshold = lib.mkOption {
          default = null;
          description = "waiting report threshold to consider waiting entries 'overdue'";
          type = lib.types.nullOr lib.types.anything;
        };
      };
    };
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
        contexts = lib.mkOption {
          default = null;
          description = "Contexts for the work report";
          type = lib.types.nullOr (lib.types.attrsOf lib.types.anything);
        };
        sorter = lib.mkOption {
          default = null;
          description = "The sorter to use to sort the rows";
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
