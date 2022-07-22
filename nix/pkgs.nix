{ sources ? import ./sources.nix
}:
import sources.nixpkgs {
  config.allowUnfree = true;
  overlays = [
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
    (import (sources.autodocodec + "/nix/overlay.nix"))
    (import (sources.fuzzy-time + "/nix/overlay.nix"))
    (import (sources.cursor-fuzzy-time + "/nix/overlay.nix"))
    (import (sources.mergeful + "/nix/overlay.nix"))
    (import (sources.yesod-static-remote + "/nix/overlay.nix"))
    (import (sources.autorecorder + "/nix/overlay.nix"))
    (import (sources.linkcheck + "/nix/overlay.nix"))
    (import (sources.seocheck + "/nix/overlay.nix"))
    (import (sources.feedback + "/nix/overlay.nix"))
    (final: previous: { niv = (import sources.niv { }).niv; })
    (final: previous: { inherit (import sources."gitignore.nix" { inherit (final) lib; }) gitignoreSource; })
    (import ./overlay.nix {
      inherit sources;
    })
  ];
}
