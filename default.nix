let
  pkgs = import ./nix/project.nix { pkgs = import ./nix/pkgs.nix { static = false; }; };
in
pkgs.smosRelease
