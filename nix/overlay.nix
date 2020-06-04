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
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                )
              )
            ) (final.haskellPackages.autoexporter)
          );
      smosPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (smosPkg name);
      smosPkgWithOwnComp = name: smosPkgWithComp name name;

    in
      {
        "smos" = smosPkgWithOwnComp "smos";
        "smos-data" = smosPkg "smos-data";
        "smos-data-gen" = smosPkg "smos-data-gen";
        "smos-cursor" = smosPkg "smos-cursor";
        "smos-cursor-gen" = smosPkg "smos-cursor-gen";
        "smos-report" = smosPkg "smos-report";
        "smos-report-gen" = smosPkg "smos-report-gen";
        "smos-report-cursor" = smosPkg "smos-report-cursor";
        "smos-report-cursor-gen" = smosPkg "smos-report-cursor-gen";
        "smos-query" = smosPkgWithOwnComp "smos-query";
        "smos-single" = smosPkgWithOwnComp "smos-single";
        "smos-scheduler" = smosPkgWithOwnComp "smos-scheduler";
        "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
        "smos-archive" = smosPkgWithOwnComp "smos-archive";
        "smos-docs-site" = smosPkg "smos-docs-site";
        "smos-api" = smosPkg "smos-api";
        "smos-api-gen" = smosPkg "smos-api-gen";
        "smos-server" = smosPkgWithOwnComp "smos-server";
        "smos-server-gen" = smosPkg "smos-server-gen";
        "smos-client" = smosPkg "smos-client";
        "smos-client-gen" = smosPkg "smos-client-gen";
        "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
        "smos-sync-client-gen" = smosPkg "smos-sync-client-gen";
        "smos-web-server" = overrideCabal (smosPkgWithOwnComp "smos-web-server") (
          old:
            {
              preBuild = ''
                ${old.preBuild or ""}
                export SMOS_WEB_SERVER_FRONT_JS=${final.smos-web-server-front}
              '';
            }
        );
      };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
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

                  typedUUIDRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "typed-uuid";
                      rev = "5415eaeee9817dfc4846fe4d73efce9312281b27";
                      sha256 =
                        "sha256:1illk01gyhhrjmz19n5wc07n61d0s2d2m348n7ibwf2795pjkrwj";
                    };

                  typedUUIDPkg =
                    name:
                      disableLibraryProfiling (
                        self.callCabal2nix name (typedUUIDRepo + "/${name}") {}
                      );

                  typedUUIDPackages =
                    final.lib.genAttrs [
                      "typed-uuid"
                      "genvalidity-typed-uuid"
                    ] typedUUIDPkg;

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
                          self.callCabal2nix name (servantAuthRepo + "/${name}") {}
                        )
                      );

                  servantAuthPackages =
                    final.lib.genAttrs [
                      "servant-auth-client"
                      "servant-auth-docs"
                      "servant-auth-server"
                      "servant-auth-swagger"
                    ] servantAuthPkg;

                  hsTlsRepo =
                    final.fetchFromGitHub {
                      owner = "ocheron";
                      repo = "hs-tls";
                      rev = "f785ce66559a09d998bcb5d459cc5ec9d53d54f0";
                      sha256 =
                        "13vq1xzwsagxdrbyl6h3fslii4jrvx7fi20h87hdqlzj3y91n1dk";
                    };
                  hsTlsPkg =
                    name: subdir:
                      dontCheck (
                        self.callCabal2nix name (hsTlsRepo + "/${subdir}") {}
                      );

                  hsTlsPackages =
                    {
                      "tls" = hsTlsPkg "tls" "core";
                      "tls-session-manager" =
                        hsTlsPkg "tls-session-manager" "session";
                      "tls-debug" = hsTlsPkg "tls-debug" "debug";
                    };

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
                      dontCheck (
                        self.callCabal2nix name (persistentRepo + "/${name}") {}
                      );
                  persistentPackages =
                    final.lib.genAttrs [
                      "persistent"
                      "persistent-template"
                      "persistent-sqlite"
                    ] persistentPkg;

                  passwordRepo =
                    final.fetchFromGitHub {
                      owner = "cdepillabout";
                      repo = "password";
                      rev = "26434d4f6888faf8dc36425b20b59f0b5056d7f5";
                      sha256 = "sha256:0kbrw7zcn687h61h574z5k8p7z671whblcrmd6q21gsa2pyrk4ll";
                    };
                  passwordPkg = name: dontCheck (self.callCabal2nix name (passwordRepo + "/${name}") {});
                  passwordPackages =
                    final.lib.genAttrs [
                      "password"
                      "password-instances"
                    ] passwordPkg;
                in
                  final.smosPackages // {
                    pantry = disableLibraryProfiling (dontCheck (self.callHackage "pantry" "0.1.1.2" {}));
                    sqlite = addBuildDepend (dontCheck (self.callCabal2nix "sqlite" sqliteRepo { sqlite = final.sqlite; })) (final.sqlite);
                    orgmode-parse = self.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
                    cron = dontCheck (self.callHackage "cron" "0.6.1" {});
                    # Passwords
                    base-64 = self.callHackage "base-64" "0.4.2" {};
                    ghc-byteorder = self.callHackage "ghc-byteorder" "4.11.0.0" {};
                  } // persistentPackages // passwordPackages // typedUUIDPackages // servantAuthPackages // hsTlsPackages
            );
        }
    );
}
