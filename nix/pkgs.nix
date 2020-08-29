let
  haskellNix = import ./haskell-nix.nix;
  pkgsv = import haskellNix.sources.nixpkgs;
  pkgs = pkgsv {};
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  linkcheck-overlay =
    import (
      pkgs.fetchFromGitHub (import ./linkcheck-version.nix) + "/nix/overlay.nix"
    );
  smosPkgs =
    pkgsv {
      config = haskellNix.config // {
        allowUnfree = true;
        allowBroken = true;
      };
      overlays =
        haskellNix.overlays ++ [
          yamlparse-applicative-overlay
          linkcheck-overlay
          (import ./gitignore-src.nix)
          (import ../smos-web-server/front/nix/overlay.nix)
          (import ./overlay.nix)
        ];
    };
in
smosPkgs
