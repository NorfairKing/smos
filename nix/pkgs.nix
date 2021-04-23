{ static ? false
}:
let
  sources = import ./sources.nix;
  pkgsv = import sources.nixpkgs;
  sydtest-overlay = import (sources.sydtest + "/nix/overlay.nix");
  validity-overlay = import (sources.validity + "/nix/overlay.nix");
  safe-coloured-text-overlay = import (sources.safe-coloured-text + "/nix/overlay.nix");
  looper-overlay = import (sources.looper + "/nix/overlay.nix");
  typed-uuid-overlay = import (sources.typed-uuid + "/nix/overlay.nix");
  pretty-relative-time-overlay = import (sources.pretty-relative-time + "/nix/overlay.nix");
  cursor-overlay = import (sources.cursor + "/nix/overlay.nix");
  cursor-brick-overlay = import (sources.cursor-brick + "/nix/overlay.nix");
  dirforest-overlay = import (sources.dirforest + "/nix/overlay.nix");
  cursor-dirforest-overlay = import (sources.cursor-dirforest + "/nix/overlay.nix");
  yamlparse-applicative-overlay = import (sources.yamlparse-applicative + "/nix/overlay.nix");
  fuzzy-time-overlay = import (sources.fuzzy-time + "/nix/overlay.nix");
  cursor-fuzzy-time-overlay = import (sources.cursor-fuzzy-time + "/nix/overlay.nix");
  mergeful-overlay = import (sources.mergeful + "/nix/overlay.nix");
  yesod-static-remote-overlay = import (sources.yesod-static-remote + "/nix/overlay.nix");
  autorecorder-overlay = import (sources.autorecorder + "/nix/overlay.nix");
  niv-overlay = final: previous:
    {
      niv = (import sources.niv { pkgs = final; }).niv;
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
          validity-overlay
          safe-coloured-text-overlay
          looper-overlay
          sydtest-overlay
          typed-uuid-overlay
          pretty-relative-time-overlay
          cursor-overlay
          cursor-brick-overlay
          dirforest-overlay
          cursor-dirforest-overlay
          fuzzy-time-overlay
          cursor-fuzzy-time-overlay
          yamlparse-applicative-overlay
          mergeful-overlay
          yesod-static-remote-overlay
          autorecorder-overlay
          niv-overlay
          gitignore-src-overlay
          (import ./overlay.nix)
        ];
        config.allowUnfree = true;
      }
    );
in
smosPkgs
