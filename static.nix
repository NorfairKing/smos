let
  pkgs = import ./nix/pkgs.nix { static = true; };
in
pkgs.smosPackages
