let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/validity-version.nix)
    + "/nix/overlay.nix")
  );
  cursor-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/cursor-version.nix)
    + "/nix/overlay.nix")
  );
  cursor-brick-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/cursor-brick-version.nix)
    + "/nix/overlay.nix")
  );
  fuzzy-time-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/fuzzy-time-version.nix)
    + "/nix/overlay.nix")
  );
  pretty-relative-time-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/pretty-relative-time-version.nix)
    + "/nix/overlay.nix")
  );
  cursor-fuzzy-time-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/cursor-fuzzy-time-version.nix)
    + "/nix/overlay.nix")
  );
  mergeful-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/mergeful-version.nix)
    + "/nix/overlay.nix")
  );
  smosPkgs = pkgsv {
  overlays =
    [ validity-overlay
      cursor-overlay
      cursor-brick-overlay
      fuzzy-time-overlay
      pretty-relative-time-overlay
      cursor-fuzzy-time-overlay
      mergeful-overlay
      (import ./nix/overlay.nix)
    ];
    config.allowUnfree = true;
  };
in smosPkgs.smosPackages // { "smos-docs-site" = smosPkgs.smosDocumentationSite; }
