final: previous:
with final.lib;
with final.haskell.lib;

let
  haskellNix = import (
    final.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "haskell.nix";
      rev = "91b0a95e4071e7998778e1fe997f8ddcf759d48b";
      sha256 = "sha256:13jx33vj8ixr3m88181wybzb8qlx0rl249bpphgz944anxp8z521";
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
          "--seed 42"
        ];

        reinstallableLibGhc = true; # Because we override the 'time' version
        packages.time.components.library.preConfigure = ''
          ${final.autoconf}/bin/autoreconf -i
        '';

        # Workaround to make the following work
        # https://github.com/input-output-hk/haskell.nix/issues/769
        # packages.smos-web-server.components.library.preBuild
        packages.smos-web-server.preBuild = mkForce ''
          export SMOS_WEB_SERVER_FRONT_JS=${final.smos-web-server-front}
        '';

        # Turn off certain test suites on macos because they generate random
        # filepaths and that fails for some reason that I cannot investigate
        # because I don't own any apple products.
        doCheck = true;
        packages.smos-report-gen.doCheck = !isMacos;
        packages.smos-query.doCheck = !isMacos;
        packages.smos-sync-client.doCheck = !isMacos;
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
            in
              ''
                mkdir -p $out/test-output
                ln -s ${testOutput} $out/test-output/${testname}
                ${lndir "${test}"}
              '';
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
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            composeExtensions (
              old.overrides or (_: _: {})
            ) (
              self: super: final.smosPackages
            );
        }
    );
}
