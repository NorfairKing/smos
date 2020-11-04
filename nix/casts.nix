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
      rev = "ad755b24009044e37c5d756d0573fa9e8ab6d71f";
      ref = "master";
    } + "/nix/cast.nix"
  ) { pkgs = pkgs // smosPackages; };
  specFiles =
    builtins.map (pkgs.lib.removeSuffix ".yaml")
      (
        builtins.attrNames
          (
            pkgs.lib.filterAttrs
              (p: v: v == "regular" && pkgs.lib.hasSuffix ".yaml" p) # TODO: filter by extension too?
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
