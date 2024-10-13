{ lib }:
{
  workflow-dir = lib.mkOption {
    default = null;
    description = "The workflow directory";
    type = lib.types.nullOr lib.types.str;
  };
}
