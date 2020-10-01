let
  pkgs = import ./nix/project.nix { pkgs = import ./nix/pkgs.nix { static = true; }; };
in
pkgs.smosRelease
