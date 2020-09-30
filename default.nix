let
  pkgs = import ./nix/pkgs.nix { static = false; };
in
pkgs.smosRelease
