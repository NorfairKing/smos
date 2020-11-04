{ pkgs
, gitignoreSource
, smosPackages
, ...
}:
let
  castsDir = ../smos-docs-site/content/casts;
  mkCastDerivation = import (
    builtins.fetchGit {
      url = "https://github.com/NorfairKing/autorecorder";
      rev = "1a2e0109a77d918242e75165da2ae4aba1afbe07";
      ref = "master";
    } + "/nix/cast.nix"
  ) { pkgs = pkgs // smosPackages; };
  specFiles =
    builtins.map (pkgs.lib.removeSuffix ".yaml")
      (
        builtins.attrNames
          (
            pkgs.lib.filterAttrs
              (p: v: v == "regular" && pkgs.lib.hasSuffix ".yaml" p)
              (builtins.readDir castsDir)
          )
      );
  derivationFor = name: mkCastDerivation {
    inherit name;
    src = ../smos-docs-site/content/casts + "/${name}.yaml";
    default-rows = 30;
    default-columns = 110;
    # debug = true;
  };
in
pkgs.lib.genAttrs specFiles derivationFor
