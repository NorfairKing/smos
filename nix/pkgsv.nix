let
  # A very specific version of nixpkgs.

  # This comes from https://github.com/srid/neuron/blob/static/nix/sources.json#L32-L33
  # See https://github.com/srid/neuron/pull/417/files
  nixpkgs-special = import (
    builtins.fetchTarball {
      url = "https://github.com/srid/nixpkgs/archive/312f5dc940b1a2c627e8cce4adc192cfa3e730db.tar.gz";
      sha256 = "1x1h6j43wp2fgzjlv0nf8h5syvpdp3nhp8xb85hxzdz8k7hkhi4s";
    }
  );
  pkgsv = nixpkgs-special;


  # Haskell.nix
  # This is the latest version at the time of writing, nothing special.
  owner = "input-output-hk";
  repo = "haskell.nix";
  rev = "48b8674f5f726cfb5083c025d3c53ff01fef009a";
  sha256 = "sha256:0b90xnxn72kv5qskp3gxfcmql8cqbank7nlp0m6353yhqp6kr5mc";
  haskellNixV = import (
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    }
  );

  haskellNix = haskellNixV { pkgs = nixpkgs-special.pkgsMusl; };

  # We need a 'clean' pkgs to use 'fetchFromGitHub' here.
  # TODO use builtins.fetchTarball here instead, to save on evaluation.
  pkgs = pkgsv {};

  # TODO get rid of the yamlparse-applicative overlay, we don't need it anymore now that we use haskell.nix
  # , I think ...
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );

  # TODO get rid of the linkcheck overlay and just use linkcheck's 'default.nix'.
  # Building its dependencies twice is fine by me if we can save on complexity.
  linkcheck-overlay =
    import (
      pkgs.fetchFromGitHub (import ./linkcheck-version.nix) + "/nix/overlay.nix"
    );


in
  # Instead of directly using the haskell.nix nixpkgs result, we want to add our own overlays here.
  # I don't think there's a way to supply one's own overlays into haskell.nix.
attrset: (
  pkgsv (
    attrset // {
      config = (attrset.config or {}) // haskellNix.config // {
        allowUnfree = true;
        allowBroken = true;
      };
      overlays =
        haskellNix.overlays
        ++ [
          yamlparse-applicative-overlay
          linkcheck-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ]
        ++ (attrset.overlays or []);
    }
  )
).pkgsMusl
