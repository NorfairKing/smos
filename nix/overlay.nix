final:
  previous:
    with final.haskell.lib;
    {
      smosPackages =
            let pathFor = name:
                  builtins.path {
                      inherit name;
                      path = ../. + "/${name}";
                      filter = path: type:
                        !(final.lib.hasPrefix "." (baseNameOf path));
                    };
                smosPkg = name:
                (failOnAllWarnings (disableLibraryProfiling (final.haskellPackages.callCabal2nix name (pathFor name) {})));
            in final.lib.genAttrs [
              "smos"
              "smos-data"
              "smos-data-gen"
              "smos-cursor"
              "smos-cursor-gen"
              "smos-report"
              "smos-report-gen"
              "smos-report-cursor"
              "smos-report-cursor-gen"
              "smos-query"
              "smos-convert-org"
              "smos-archive"
              "smos-docs-site"
            ] smosPkg;
      smosDocumentationSite =  final.stdenv.mkDerivation rec {
          name = "smosDocumentationSite";
          src = ../smos-docs-site;
          phases = "unpackPhase buildPhase";
          version = "0.0";
          buildInputs = [
            final.haskellPackages.smos-docs-site
          ];
          buildPhase = ''
            export LOCALE_ARCHIVE="${final.glibcLocales}/lib/locale/locale-archive";
            export LANG=en_US.UTF-8
            smos-docs-site build
            
            mkdir $out
            cp -r _site/* $out
          '';
        };
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
                hakyll-sass = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "hakyll-sass" "0.2.3" {});
                orgmode-parse = super.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
              }
        );
      });
    }
