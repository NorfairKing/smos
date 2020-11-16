{ static ? false
}:
let
  pkgsv = import (import ./nixpkgs.nix);
  validity-overlay =
    import (
      builtins.fetchGit (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  typed-uuid-overlay =
    import (
      builtins.fetchGit (import ./typed-uuid-version.nix) + "/nix/overlay.nix"
    );
  pretty-relative-time-overlay =
    import (
      builtins.fetchGit (import ./pretty-relative-time-version.nix) + "/nix/overlay.nix"
    );
  cursor-overlay =
    import (
      builtins.fetchGit (import ./cursor-version.nix) + "/nix/overlay.nix"
    );
  cursor-brick-overlay =
    import (
      builtins.fetchGit (import ./cursor-brick-version.nix) + "/nix/overlay.nix"
    );
  dirforest-overlay =
    import (
      builtins.fetchGit (import ./dirforest-version.nix) + "/nix/overlay.nix"
    );
  cursor-dirforest-overlay =
    import (
      builtins.fetchGit (import ./cursor-dirforest-version.nix) + "/nix/overlay.nix"
    );
  fuzzy-time-overlay =
    import (
      builtins.fetchGit (import ./fuzzy-time-version.nix) + "/nix/overlay.nix"
    );
  cursor-fuzzy-time-overlay =
    import (
      builtins.fetchGit (import ./cursor-fuzzy-time-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      builtins.fetchGit (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  mergeful-overlay =
    import (
      builtins.fetchGit (import ./mergeful-version.nix) + "/nix/overlay.nix"
    );
  yesod-static-remote-overlay =
    import (
      builtins.fetchGit (import ./yesod-static-remote-version.nix) + "/nix/overlay.nix"
    );
  autorecorder-overlay =
    import (
      builtins.fetchGit (import ./autorecorder-version.nix) + "/nix/overlay.nix"
    );
  pkgFunc = pkgs: if static then pkgs.pkgsCross.musl64 else pkgs;
  smosPkgs = pkgFunc
    (
      pkgsv {
        overlays = [
          validity-overlay
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
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
        config.allowUnfree = true;
      }
    );
in
smosPkgs
