let
  pkgsv = import (import ./nixpkgs.nix);
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
      overlays =
        [
          yamlparse-applicative-overlay
          linkcheck-overlay
          (import ./gitignore-src.nix)
          (import ../smos-web-server/front/nix/overlay.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
      config.allowBroken = true;
    };
in
smosPkgs
