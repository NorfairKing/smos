let
  pkgs = import ./pkgs.nix { static = true; };
in
pkgs # .smosPackages
