let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  cursor-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-version.nix) + "/nix/overlay.nix"
    );
  cursor-brick-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-brick-version.nix) + "/nix/overlay.nix"
    );
  fuzzy-time-overlay =
    import (
      pkgs.fetchFromGitHub (import ./fuzzy-time-version.nix) + "/nix/overlay.nix"
    );
  pretty-relative-time-overlay =
    import (
      pkgs.fetchFromGitHub (import ./pretty-relative-time-version.nix) + "/nix/overlay.nix"
    );
  cursor-fuzzy-time-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-fuzzy-time-version.nix) + "/nix/overlay.nix"
    );
  dirforest-overlay =
    import (
      pkgs.fetchFromGitHub (import ./dirforest-version.nix) + "/nix/overlay.nix"
    );
  cursor-dirforest-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-dirforest-version.nix) + "/nix/overlay.nix"
    );
  mergeful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeful-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  smosPkgs =
    pkgsv {
      overlays =
        [
          validity-overlay
          cursor-overlay
          cursor-brick-overlay
          fuzzy-time-overlay
          pretty-relative-time-overlay
          cursor-fuzzy-time-overlay
          dirforest-overlay
          cursor-dirforest-overlay
          mergeful-overlay
          yamlparse-applicative-overlay
          (import ./gitignore-src.nix)
          (import ../smos-web-server/front/nix/overlay.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
      config.allowBroken = true;
    };
in
smosPkgs
