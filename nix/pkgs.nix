{ static ? false
}:
let
  pkgsv = import ./pkgsv.nix { inherit static; };
  pkgs = pkgsv {};
in
pkgs
