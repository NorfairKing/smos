final:
  previous:
    with final.haskell.lib;
    {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super:
            let smosPkg = name:
                (buildStrictly (self.callCabal2nix name (../. + "/${name}") {}));
            in final.lib.genAttrs [
              "smos"
              "smos-data"
              "smos-data-gen"
              "smos-cursor"
              "smos-cursor-gen"
            ] smosPkg
        );
      });
    }
