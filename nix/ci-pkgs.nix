let
  pkgsv = import ./pkgsv.nix;
  pkgs = pkgsv { overlays = [ (import ./ci-overlay.nix) ]; };
in
pkgs
