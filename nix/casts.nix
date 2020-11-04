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
      rev = "b6d411946258bded132762d2c1e0c6ba256d01a2";
      ref = "master";
    } + "/nix/cast.nix"
    # /home/syd/src/autorecorder/nix/cast.nix
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
