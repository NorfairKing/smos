final:
  previous:
    with final.haskell.lib;
    {
      smosPackages =
            let smosPkg = name:
                (failOnAllWarnings (final.haskellPackages.callCabal2nix name (../. + "/${name}") {}));
            in final.lib.genAttrs [
              "smos"
              "smos-data"
              "smos-data-gen"
              "smos-cursor"
              "smos-cursor-gen"
              "smos-report"
              "smos-report-gen"
            ] smosPkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.smosPackages
        );
      });
    }
