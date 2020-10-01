let
  pkgs = import ./pkgs.nix { static = false; };
  nixBuildUncached = pkgs.fetchFromGitHub {
    owner = "Mic92";
    repo = "nix-build-uncached";
    rev = "611b17999fb1ddbf243a120d96a1232a320f1704";
    sha256 = "sha256:1snwsmn8qcbvilhn0wqspqz3zkb0dhrb8bpbclbazi94dsgksxjy";
  };
in
import (nixBuildUncached + "/default.nix") { inherit pkgs; }
