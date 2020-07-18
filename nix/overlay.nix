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
      sPkgs = import ../stack-to-nix/default.nix {};
      smosPkg =
        name: sPkgs."${name}";
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
              old.overrides or (_: _: {})
            ) (
              self: super: final.smosPackages
            );
        }
    );
}
