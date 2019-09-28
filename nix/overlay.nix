final: previous:
with final.haskell.lib;

{
  smosPackages =
    let
      smosPkg =
        name:
          addBuildDepend (
            failOnAllWarnings (
              disableLibraryProfiling (
                final.haskellPackages.callCabal2nix name ( final.gitignoreSource ( ../. + "/${name}" ) ) {}
              )
            )
          ) ( final.haskellPackages.autoexporter );
    in
      final.lib.genAttrs [
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
        "smos-single"
        "smos-convert-org"
        "smos-archive"
        "smos-docs-site"
        "smos-sync-api"
        "smos-sync-api-gen"
        "smos-sync-server"
        "smos-sync-server-gen"
        "smos-sync-client"
        "smos-sync-client-gen"
      ] smosPkg;
  smosDocumentationSite =
    final.stdenv.mkDerivation rec {
      name = "smosDocumentationSite";
      src = final.gitignoreSource ../smos-docs-site;
      phases = "unpackPhase buildPhase";
      version = "0.0";
      buildInputs =
        [
          ( final.haskellPackages.smos-docs-site )
        ];
      buildPhase =
        ''
          export LOCALE_ARCHIVE="${final.glibcLocales}/lib/locale/locale-archive";
          export LANG=en_US.UTF-8
          smos-docs-site build
          smos-docs-site check --internal-links
          
          mkdir $out
          cp -r _site/* $out
        '';
    };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (_:
            _:
              {})
            ) (
              self: super:
                let
                  orgmodeParseRepo =
                    final.fetchFromGitHub {
                      owner = "ixmatus";
                      repo = "orgmode-parse";
                      rev = "1bdfbfe8fb7299724a6f6a122a93b2e96dd839f8";
                      sha256 =
                        "0czqqvib9wndhyh18n20ckny2xyn9f7cr6bmrkzspl0aligkb3rv";
                    };
                in
                  final.smosPackages // {
                hakyll = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "hakyll" "4.13.0.0" {});
                hakyll-sass = final.haskell.lib.dontCheck (final.haskellPackages.callHackage "hakyll-sass" "0.2.4" {});
                orgmode-parse = super.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
              }
            );
        }
    );
}
