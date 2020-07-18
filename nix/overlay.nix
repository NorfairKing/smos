final: previous:
with final.haskell.lib;

{
  smosRelease =
    final.stdenv.mkDerivation {
      name = "smos-release";
      buildInputs = final.lib.attrsets.attrValues final.smosPackages;
      # Just to make sure that the test suites in these pass:
      nativeBuildInputs =
        final.lib.attrsets.attrValues final.validityPackages
        ++ final.lib.attrsets.attrValues final.cursorPackages
        ++ final.lib.attrsets.attrValues final.cursorBrickPackages
        ++ final.lib.attrsets.attrValues final.fuzzyTimePackages
        ++ final.lib.attrsets.attrValues final.cursorFuzzyTimePackages
        ++ final.lib.attrsets.attrValues final.prettyRelativeTimePackages
        ++ final.lib.attrsets.attrValues final.mergefulPackages;
      buildCommand =
        ''
          mkdir -p $out/bin
          for i in $buildInputs
          do
            if [ -d "$i/bin" ]
            then
              cp $i/bin/* $out/bin/
            fi
          done
        '';
    };

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

      docsSite =
        let
          rawDocsSite = smosPkg "smos-docs-site";
        in
          final.stdenv.mkDerivation {
            name = "smos-docs-site";
            buildInputs = [ final.haskellPackages.linkcheck final.killall ];
            buildCommand = ''
              mkdir -p $out
              cp -r ${rawDocsSite}/. $out

              $out/bin/smos-docs-site &
              sleep 1
              linkcheck http://localhost:8000
              killall smos-docs-site
            '';
          };

    in
      {
        "smos" = smosPkgWithOwnComp "smos";
        "smos-data" = smosPkg "smos-data";
        "smos-data-gen" = smosPkg "smos-data-gen";
        "smos-cursor" = smosPkg "smos-cursor";
        "smos-cursor-gen" = smosPkg "smos-cursor-gen";
        "smos-report" = smosPkg "smos-report";
        "smos-report-gen" =
          let
            default = smosPkg "smos-report-gen";
            set = {
              "x86_64-darwin" = dontCheck default; # Because it hangs
            };
          in
            set."${builtins.currentSystem}" or default;
        "smos-report-cursor" = smosPkg "smos-report-cursor";
        "smos-report-cursor-gen" = smosPkg "smos-report-cursor-gen";
        "smos-query" =
          let
            default = smosPkgWithOwnComp "smos-query";
            set = {
              "x86_64-darwin" = dontCheck default;
            };
          in
            set."${builtins.currentSystem}" or default;
        "smos-single" = smosPkgWithOwnComp "smos-single";
        "smos-scheduler" = smosPkgWithOwnComp "smos-scheduler";
        "smos-archive" = smosPkgWithOwnComp "smos-archive";
        "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
        "smos-calendar-import" = smosPkgWithOwnComp "smos-calendar-import";
        "smos-asciinema" = smosPkgWithOwnComp "smos-asciinema";
        "smos-docs-site" = docsSite;
        "smos-api" = smosPkg "smos-api";
        "smos-api-gen" = smosPkg "smos-api-gen";
        "smos-server" = smosPkgWithOwnComp "smos-server";
        "smos-server-gen" = smosPkg "smos-server-gen";
        "smos-client" = smosPkg "smos-client";
        "smos-client-gen" = smosPkg "smos-client-gen";
        "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
        "smos-sync-client-gen" =
          let
            default = smosPkg "smos-sync-client-gen";
            set = {
              "x86_64-darwin" = dontCheck default;
            };
          in
            set."${builtins.currentSystem}" or default;
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

                  # Passwords
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
                  iCalendarRepo =
                    final.fetchFromGitHub {
                      owner = "chrra";
                      repo = "iCalendar";
                      rev = "66b408f10b2d87929ecda715109b26093c711823";
                      sha256 = "sha256:1qipvvcan5ahx3a16davji7b21m09s2jdxm78q75hxk6bk452l37";
                    };
                  iCalendarPkg = dontCheck (self.callCabal2nix "iCalendar" iCalendarRepo {});
                  timeRepo =
                    final.fetchFromGitHub {
                      owner = "haskell";
                      repo = "time";
                      rev = "8ffb3da1118ddd40cbb2bc3cd8cf4a9d94d15211";
                      sha256 = "sha256:1qipvvcan5ahx3a16davji7b21m09s2jdxm78q75hxk6bk452aaa";
                    };
                  timePkg = final.callPackage ./time.nix (
                    {
                      mkDerivation = final.stdenv.mkDerivation;
                      base = final.haskellPackages.base;
                      inherit (final.haskellPackages)
                        deepseq
                        QuickCheck
                        random
                        tasty
                        tasty-hunit
                        tasty-quickcheck
                        unix
                        ;
                    }
                  ); # dontCheck (self.callCabal2nix "time" timeRepo {});
                in
                  final.smosPackages // {
                    # directory = self.callHackage "directory" "1.3.6" {};
                    # process = self.callHackage "process" "1.6.10" {};
                    # unix = self.callHackage "unix" "2.7.2.2" {};

                    envparse = self.callHackage "envparse" "0.4.1" {};
                    time = timePkg;
                    # time = self.callHackage "time" "1.10.0" {};
                    # Cabal = self.callHackage "Cabal" "3.2.0" {};

                    servant-flatten = self.callHackage "servant-flatten" "0.2" {};

                    sqlite = addBuildDepend (dontCheck (self.callCabal2nix "sqlite" sqliteRepo { sqlite = final.sqlite; })) (final.sqlite);
                    orgmode-parse = self.callCabal2nix "orgmode-parse" orgmodeParseRepo {};
                    cron = dontCheck (self.callHackage "cron" "0.6.1" {});
                    # Passwords
                    ghc-byteorder = self.callHackage "ghc-byteorder" "4.11.0.0" {};
                    # Calendar
                    iCalendar = iCalendarPkg;
                    mime = self.callHackage "mime" "0.4.0.2" {};
                    genvalidity-dirforest = dontCheck super.genvalidity-dirforest;
                  } // passwordPackages // typedUUIDPackages // servantAuthPackages
            );
        }
    );
}
