let
  pkgs = import ../nix/pkgs.nix;
in
pkgs.stdenv.mkDerivation rec {
  name = "smos-demo-workflow-shell";

  buildInputs = with pkgs.smosPackages; [
    smos
    smos-single
    smos-query
    smos-archive
    smos-sync-client
  ];

  shellHook = ''
    export SMOS_WORKFLOW_DIR=$(pwd)
    export SMOS_CONFIG_FILE=$(pwd)/config.yaml
  '';

}
