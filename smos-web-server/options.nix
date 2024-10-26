{ lib }:
{
  api-url = lib.mkOption {
    default = null;
    description = "The url for the api to use";
    type = lib.types.nullOr lib.types.str;
  };
  data-dir = lib.mkOption {
    default = null;
    description = "The directory to store workflows during editing";
    type = lib.types.nullOr lib.types.str;
  };
  docs-url = lib.mkOption {
    default = null;
    description = "The url to the docs site to refer to";
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
  port = lib.mkOption {
    default = null;
    description = "The port to serve web requests on";
    type = lib.types.nullOr lib.types.int;
  };
  web-url = lib.mkOption {
    default = null;
    description = "The url that this web server is served from";
    type = lib.types.nullOr lib.types.str;
  };
}
