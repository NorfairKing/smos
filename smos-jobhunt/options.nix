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
  goal = lib.mkOption {
    default = null;
    description = "The goal for initialised projects";
    type = lib.types.nullOr lib.types.anything;
  };
  jobhunt = lib.mkOption {
    default = { };
    type = lib.types.submodule {
      options = {
        directory = lib.mkOption {
          default = null;
          description = "The directory to put jobhunt projects in, relative to the projects dir";
          type = lib.types.nullOr lib.types.str;
        };
        email = lib.mkOption {
          default = { };
          type = lib.types.submodule {
            options = {
              from = lib.mkOption {
                default = { };
                type = lib.types.submodule {
                  options = {
                    address = lib.mkOption {
                      default = null;
                      description = "From address";
                      type = lib.types.nullOr lib.types.str;
                    };
                    name = lib.mkOption {
                      default = null;
                      description = "From name";
                      type = lib.types.nullOr lib.types.str;
                    };
                  };
                };
              };
              smtp = lib.mkOption {
                default = { };
                type = lib.types.submodule {
                  options = {
                    password = lib.mkOption {
                      default = null;
                      description = "SMTP server password";
                      type = lib.types.nullOr lib.types.str;
                    };
                    password-file = lib.mkOption {
                      default = null;
                      description = "SMTP server password file";
                      type = lib.types.nullOr lib.types.str;
                    };
                    port = lib.mkOption {
                      default = null;
                      description = "SMTP server port";
                      type = lib.types.nullOr lib.types.ints.u16;
                    };
                    server = lib.mkOption {
                      default = null;
                      description = "SMTP server domain";
                      type = lib.types.nullOr lib.types.str;
                    };
                    username = lib.mkOption {
                      default = null;
                      description = "SMTP server username";
                      type = lib.types.nullOr lib.types.str;
                    };
                  };
                };
              };
              template = lib.mkOption {
                default = { };
                type = lib.types.submodule {
                  options = {
                    html = lib.mkOption {
                      default = null;
                      description = "Template for the HTML version of the email";
                      type = lib.types.nullOr lib.types.str;
                    };
                    subject = lib.mkOption {
                      default = null;
                      description = "Template for the subject of the email";
                      type = lib.types.nullOr lib.types.str;
                    };
                    text = lib.mkOption {
                      default = null;
                      description = "Template for the text version of the email";
                      type = lib.types.nullOr lib.types.str;
                    };
                  };
                };
              };
            };
          };
        };
        log-level = lib.mkOption {
          default = null;
          description = "Minimal severity of log messages";
          type = lib.types.nullOr lib.types.anything;
        };
        waiting-threshold = lib.mkOption {
          default = null;
          description = "The waiting threshold initialised projects";
          type = lib.types.nullOr lib.types.anything;
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
