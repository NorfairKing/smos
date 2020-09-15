let
  pkgs = import ../../nix/ci-pkgs.nix;
in
pkgs.stdenv.mkDerivation rec {
  name = "smos-demo-workflow-shell";

  buildInputs = with pkgs.smosPackages; [
    smos
    smos-single
    smos-scheduler
    smos-query
    smos-archive
    smos-sync-client
    smos-calendar-import
  ];

  shellHook = ''
    export SMOS_WORKFLOW_DIR=$(pwd)
    export SMOS_CONFIG_FILE=$(pwd)/config.yaml
    export SMOS_EXPLAINER_MODE=True

    # Sync setup
    export SMOS_CONTENTS_DIR=$(pwd)
    export SMOS_USERNAME=exampleuser
    export SMOS_PASSWORD=examplepass
    export SMOS_SERVER_URL=localhost:8001
    export SMOS_UUID_FILE=$(pwd)/../server.uuid
    export SMOS_SESSION_PATH=$(pwd)/../session.json
    export SMOS_METADATA_DATABASE=$(pwd)/../sync-metadata.sqlite3
  '';

}
