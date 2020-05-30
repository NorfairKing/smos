let
  pkgsv = import (import ../../../nix/nixpkgs.nix);
  pkgs = pkgsv {};
  smosFrontPkgs =
    pkgsv {
      overlays =
        [
          (import ../../../nix/gitignore-src.nix)
          (import ./overlay.nix)
        ];
    };
in
smosFrontPkgs
