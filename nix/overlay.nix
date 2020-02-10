final: previous:
with final.haskell.lib;

{
  smosPackages =
    let
      smosPkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name ( final.gitignoreSource ( ../. + "/${name}" ) ) {}
                )
              )
            ) ( final.haskellPackages.autoexporter )
          );
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
        "smos-scheduler"
        "smos-convert-org"
        "smos-archive"
        "smos-docs-site"
        "smos-api"
        "smos-api-gen"
        "smos-server"
        "smos-server-gen"
        "smos-client"
        "smos-client-gen"
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
                  sqliteRepo =
                    final.fetchFromGitHub {
                      owner = "GaloisInc";
                      repo = "sqlite";
                      rev = "e93ee84000c1d1eedbc23036c4a20ffd07e3145f";
                      sha256 =
                        "1ia3i97lcpsgi4zmk67hi2f2crffpiqndhl11dllw1mkqr92hklk";
                    };

                  servantAuthRepo =
                    final.fetchFromGitHub {
                      owner = "haskell-servant";
                      repo = "servant-auth";
                      rev = "62d3f4b6a7fd7dc38510d4c60982239f94fc1b58";
                      sha256 =
                        "sha256:0syp5k2nm1jb1lh3z1ajzpgq35jhbm8qx3xr22s5qv27f6y7f99v";
                    };

                  servantAuthPkg =
                    name:
                      disableLibraryProfiling (
                        dontCheck (
                          self.callCabal2nix name ( servantAuthRepo + "/${name}" ) {}
                        )
                      );
                  servantAuthPackages =
                    final.lib.genAttrs [
                      "servant-auth-client"
                      "servant-auth-docs"
                      "servant-auth-server"
                      "servant-auth-swagger"
                    ] servantAuthPkg;

                  persistentRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "persistent";
                      rev = "0e3bbb1bd2f5f6383f9eb0407a2416e8b12255ee";
                      sha256 =
                        "1nzl7kckbvarffj96695xh4wg3d387fyhzxp3sbsv0jzh2iv1kj7";
                    };

                  persistentPkg =
                    name:
                      disableLibraryProfiling (
                        dontCheck (
                          self.callCabal2nix name ( persistentRepo + "/${name}" ) {}
                        )
                      );
                  persistentPackages =
                    final.lib.genAttrs [
                      "persistent"
                      "persistent-template"
                      "persistent-sqlite"
                    ] persistentPkg;
                in
                  final.smosPackages // {
                hakyll = dontCheck (self.callHackage "hakyll" "4.13.0.1" {});
                hakyll-sass = dontCheck (self.callHackage "hakyll-sass" "0.2.4" {});
                warp = dontCheck (self.callHackage "warp" "3.2.28" {});
                http2 = dontCheck (self.callHackage "http2" "1.6.5" {});
                pantry = disableLibraryProfiling (dontCheck (self.callHackage "pantry" "0.1.1.2" {}));
                sqlite = addBuildDepend (dontCheck (self.callCabal2nix "sqlite" sqliteRepo { sqlite = final.sqlite; })) (final.sqlite) ;
                orgmode-parse = self.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
                cron = dontCheck (self.callHackage "cron" "0.6.1" {});
              } // persistentPackages // servantAuthPackages
            );
        }
    );
}
