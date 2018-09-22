let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/validity-version.nix)
    + "/nix/overlay.nix")
  );
  cursor-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/cursor-version.nix)
    + "/nix/overlay.nix")
  );
in pkgsv {
  overlays = [ validity-overlay cursor-overlay (import ./nix/overlay.nix) ];
  config.allowUnfree = true;
}
