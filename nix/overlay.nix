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
              "smos-query"
              "smos-convert-org"
            ] smosPkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
        self: super: 
          let
            orgmodeParseRepo = final.fetchFromGitHub {
                owner = "ixmatus";
                repo = "orgmode-parse";
                rev = "1bdfbfe8fb7299724a6f6a122a93b2e96dd839f8";
                sha256 = "0czqqvib9wndhyh18n20ckny2xyn9f7cr6bmrkzspl0aligkb3rv";
              };

            in final.smosPackages //
              { # Have to turn off tests because they don't compile --'
                thyme = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "thyme" "0.3.5.5" {});
                orgmode-parse = super.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
              }
        );
      });
    }
