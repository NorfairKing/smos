{ static ? false
, sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
  niv-overlay = final: previous:
    {
      niv = (import sources.niv { }).niv;
    };
  gitignore-src-overlay = final: previous:
    {
      inherit (import sources."gitignore.nix" { inherit (final) lib; }) gitignoreSource;
    };
  pkgFunc = pkgs: if static then pkgs.pkgsCross.musl64 else pkgs;
  smosPkgs = pkgFunc
    (
      pkgsv {
        overlays = [
          gitignore-src-overlay
          (import (sources.sydtest + "/nix/overlay.nix"))
          (import (sources.validity + "/nix/overlay.nix"))
          (import (sources.safe-coloured-text + "/nix/overlay.nix"))
          (import (sources.looper + "/nix/overlay.nix"))
          (import (sources.typed-uuid + "/nix/overlay.nix"))
          (import (sources.pretty-relative-time + "/nix/overlay.nix"))
          (import (sources.cursor + "/nix/overlay.nix"))
          (import (sources.cursor-brick + "/nix/overlay.nix"))
          (import (sources.dirforest + "/nix/overlay.nix"))
          (import (sources.cursor-dirforest + "/nix/overlay.nix"))
          (import (sources.yamlparse-applicative + "/nix/overlay.nix"))
          (import (sources.fuzzy-time + "/nix/overlay.nix"))
          (import (sources.cursor-fuzzy-time + "/nix/overlay.nix"))
          (import (sources.mergeful + "/nix/overlay.nix"))
          (import (sources.yesod-static-remote + "/nix/overlay.nix"))
          (import (sources.autorecorder + "/nix/overlay.nix"))
          niv-overlay
          (import ./overlay.nix { inherit sources; })
        ];
        config.allowUnfree = true;
      }
    );
in
smosPkgs
