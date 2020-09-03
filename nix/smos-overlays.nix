let
  haskellNix = import ./haskell-nix.nix;
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  linkcheck-overlay =
    import (
      pkgs.fetchFromGitHub (import ./linkcheck-version.nix) + "/nix/overlay.nix"
    );
in
haskellNix.overlays ++ [
  yamlparse-applicative-overlay
  linkcheck-overlay
  (import ./gitignore-src.nix)
  (import ./overlay.nix)
];
