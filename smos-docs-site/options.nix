{ lib }:
{
  api-url = lib.mkOption {
    default = null;
    description = "The url for the api to use";
    type = lib.types.nullOr lib.types.str;
  };
  google-analytics-tracking = lib.mkOption {
    default = null;
    description = "The Google analytics tracking code";
    type = lib.types.nullOr lib.types.str;
  };
  google-search-console-verification = lib.mkOption {
    default = null;
    description = "The Google search console verification code";
    type = lib.types.nullOr lib.types.str;
  };
  log-level = lib.mkOption {
    default = null;
    description = "Minimal severity of log messages";
    type = lib.types.nullOr lib.types.anything;
  };
  necrork = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        intray = lib.mkOption {
          default = { };
          type = lib.types.submodule {
            options = {
              key = lib.mkOption {
                default = null;
                description = "Access key";
                type = lib.types.nullOr lib.types.str;
              };
              username = lib.mkOption {
                default = null;
                description = "Username";
                type = lib.types.nullOr lib.types.str;
              };
            };
          };
        };
        notifier = lib.mkOption {
          default = { };
          type = lib.types.submodule {
            options = {
              enable = lib.mkOption {
                default = null;
                description = "enable the notifier looper";
                type = lib.types.nullOr lib.types.bool;
              };
              period = lib.mkOption {
                default = null;
                description = "period of the notifier looper in seconds";
                type = lib.types.nullOr lib.types.number;
              };
              phase = lib.mkOption {
                default = null;
                description = "phase of the notifier looper in seconds";
                type = lib.types.nullOr lib.types.number;
              };
            };
          };
        };
        switch = lib.mkOption {
          default = null;
          description = "Name of the necrork switch";
          type = lib.types.nullOr lib.types.str;
        };
        timeout = lib.mkOption {
          default = null;
          description = "How long after last hearing from this switch, nodes should consider it dead";
          type = lib.types.nullOr lib.types.ints.u32;
        };
        url = lib.mkOption {
          default = null;
          description = "Base url of the necrork server";
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
  web-url = lib.mkOption {
    default = null;
    description = "The url for the web server to refer to";
    type = lib.types.nullOr lib.types.str;
  };
}
