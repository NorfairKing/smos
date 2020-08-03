final: previous:
with final.lib;
with final.haskell.lib;

let
  haskellNix = import (
    final.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "haskell.nix";
      rev = "f4136211c933b444ab2e0f358abd223929970220";
      sha256 = "sha256:1b9nxzkg29hwczr6pb6a7arxka8z0swzq7b2bqyxqzr4qvpcjlc1";
    }
  ) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  haskellNixPkgs = import nixpkgsSrc nixpkgsArgs;
  isMacos = builtins.currentSystem == "x86_64-darwin";
  sPkgs = haskellNixPkgs.haskell-nix.stackProject {
    src = final.gitignoreSource ../.;
    modules = [
      {
        testFlags = [
          "--seed 42" # To make sure the tests are reproducable
        ];

        reinstallableLibGhc = true; # Because we override the 'time' version
        packages.time.components.library.preConfigure = ''
          ${final.autoconf}/bin/autoreconf -i
        '';

        # The smos web server front-end.
        packages.smos-web-server.components.library.preBuild = ''
          export SMOS_WEB_SERVER_FRONT_JS=${final.smos-web-server-front}
        '';

        # Turn off certain test suites on macos because they generate random
        # filepaths and that fails for some reason that I cannot investigate
        # because I don't own any apple products.
        doCheck = true;
        packages.smos-archive.doCheck = !isMacos;
        packages.smos-cursor-gen.doCheck = !isMacos;
        packages.smos-query.doCheck = !isMacos;
        packages.smos-report-gen.doCheck = !isMacos;
        packages.smos-scheduler.doCheck = !isMacos;
        packages.smos-sync-client-gen.doCheck = !isMacos;
        packages.smos.doCheck = !isMacos;

      }
      # Set the pedantic build up with https://github.com/input-output-hk/haskell.nix/issues/519 when that works.
      {
        packages = {
          smos.package.ghcOptions = "-Werror";
          smos-data.package.ghcOptions = "-Werror";
          smos-data-gen.package.ghcOptions = "-Werror";
          smos-cursor.package.ghcOptions = "-Werror";
          smos-cursor-gen.package.ghcOptions = "-Werror";
          smos-report.package.ghcOptions = "-Werror";
          smos-report-gen.package.ghcOptions = "-Werror";
          smos-report-cursor.package.ghcOptions = "-Werror";
          smos-report-cursor-gen.package.ghcOptions = "-Werror";
          smos-query.package.ghcOptions = "-Werror";
          smos-single.package.ghcOptions = "-Werror";
          smos-scheduler.package.ghcOptions = "-Werror";
          smos-archive.package.ghcOptions = "-Werror";
          smos-convert-org.package.ghcOptions = "-Werror";
          smos-calendar-import.package.ghcOptions = "-Werror";
          smos-asciinema.package.ghcOptions = "-Werror";
          smos-api.package.ghcOptions = "-Werror";
          smos-api-gen.package.ghcOptions = "-Werror";
          smos-server.package.ghcOptions = "-Werror";
          smos-server-gen.package.ghcOptions = "-Werror";
          smos-client.package.ghcOptions = "-Werror";
          smos-client-gen.package.ghcOptions = "-Werror";
          smos-sync-client.package.ghcOptions = "-Werror";
          smos-sync-client-gen.package.ghcOptions = "-Werror";
          smos-web-server.package.ghcOptions = "-Werror";
          smos-docs-site.package.ghcOptions = "-Werror";
        };
      }
    ];
  };

  # Until this is built-in to the haskell.nix workings.
  # https://github.com/input-output-hk/haskell.nix/issues/624
  completionsFor = exeName: exe:
    final.stdenv.mkDerivation {
      name = "${exeName}-completion";
      buildCommand =
        ''
          bashCompDir="$out/share/bash-completion/completions"
          zshCompDir="$out/share/zsh/vendor-completions"
          fishCompDir="$out/share/fish/vendor_completions.d"
          mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
          "${exe}/bin/${exeName}" --bash-completion-script "${exe}/bin/${exeName}" >"$bashCompDir/${exeName}"
          "${exe}/bin/${exeName}" --zsh-completion-script  "${exe}/bin/${exeName}" >"$zshCompDir/_${exeName}"
          "${exe}/bin/${exeName}" --fish-completion-script "${exe}/bin/${exeName}" >"$fishCompDir/${exeName}.fish"
          # Sanity check
          grep -F ${exeName} <$bashCompDir/${exeName} >/dev/null || {
            echo 'Could not find ${exeName} in completion script.'
            exit 1
          }
        '';
    };
  smosPkg = name:
    with sPkgs."${name}".components;
    final.stdenv.mkDerivation {
      name = "${name}";
      buildInputs = [ library ];
      buildCommand =
        let
          testCommand = testname: test:
            let
              testOutput = haskellNixPkgs.haskell-nix.haskellLib.check test;
              testOutputCommand = optionalString test.config.doCheck
                ''
                  mkdir -p $out/test-output
                  ln -s ${testOutput} $out/test-output/${testname}
                '';
              testLinkCommand = lndir "${test}";
            in
              concatStringsSep "\n" [ testOutputCommand testLinkCommand ];
          benchCommand = benchname: bench: lndir "${bench}";
          exeCommand = exename: exe: lndir "${exe}";
          lndir = dir: "${final.xorg.lndir}/bin/lndir -silent ${dir} $out";
        in
          ''
            mkdir -p $out
            for i in $buildInputs
            do
              ${lndir "$i"}
            done
            ${concatStringsSep "\n" (mapAttrsToList testCommand tests)}
            ${concatStringsSep "\n" (mapAttrsToList benchCommand benchmarks)}
            rm -rf $out/bin # Don't keep any bins from the tests or benchmarks
            ${concatStringsSep "\n" (mapAttrsToList exeCommand exes)}
            rm -rf $out/{exactDep,envDep,lib,package.conf.d} # Don't keep files we don't need.
          '';
    };
  smosPkgWithComp = exeName: name:
    final.symlinkJoin {
      name = "${exeName}-with-completion";
      paths = [
        (smosPkg name)
        (completionsFor exeName (smosPkg name))
      ];
    };
  smosPkgWithOwnComp = name: smosPkgWithComp name name;
in
{
  smosRelease =
    final.symlinkJoin {
      name = "smos-release";
      paths = attrValues final.smosPackages;
    };

  smosPackages =
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
      "smos-archive" = smosPkgWithOwnComp "smos-archive";
      "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
      "smos-calendar-import" = smosPkgWithOwnComp "smos-calendar-import";
      "smos-asciinema" = smosPkgWithOwnComp "smos-asciinema";
      "smos-api" = smosPkg "smos-api";
      "smos-api-gen" = smosPkg "smos-api-gen";
      "smos-server" = smosPkgWithOwnComp "smos-server";
      "smos-server-gen" = smosPkg "smos-server-gen";
      "smos-client" = smosPkg "smos-client";
      "smos-client-gen" = smosPkg "smos-client-gen";
      "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
      "smos-sync-client-gen" = smosPkg "smos-sync-client-gen";
      "smos-web-server" = smosPkgWithOwnComp "smos-web-server";
      "smos-docs-site" =
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
    };

  smosCasts = import ./casts.nix final;
}
