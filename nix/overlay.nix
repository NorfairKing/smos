final: previous:
with final.lib;

let
  isMacos = builtins.currentSystem == "x86_64-darwin";
  sPkgs = final.haskell-nix.stackProject {
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

        # A smos mime setup. This allows users to open smos files with xdg-open.
        packages.smos.components.library.postBuild = ''
          # Set up the types
          mkdir -p $out/share/mime/packages
          cp ${../mime/smos.mime-type} $out/share/mime/packages/smos.xml

          # Set up the .desktop files
          mkdir -p $out/share/applications
          cp ${../mime/smos.desktop} $out/share/applications/smos.desktop
        '';

        # The smos web server front-end.
        #
        # We put these remote assets in place before the build so that they do not get fetched during the build
        packages.smos-web-server.components.library.preBuild =
          let
            bulmaCSS = builtins.fetchurl {
              url = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css";
              sha256 = "sha256:0lhpzahlszi5nr82n3sny5fjk4k1vaq11rdrddjmka23np53klqg";
            };
            faviconICO = builtins.fetchurl {
              url = "https://cs-syd.eu/logo/res/favicon.ico";
              sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
            };
            jqueryJS = builtins.fetchurl {
              url = "https://code.jquery.com/jquery-3.3.1.min.js";
              sha256 = "sha256:1vq2bp290rhby5l09dv5khqwv3ysnzbddggbgk6m4hl9y9pl42hn";
            };
            xtermJS = builtins.fetchurl {
              url = "https://cdn.jsdelivr.net/npm/xterm@4.8.1/lib/xterm.min.js";
              sha256 = "sha256:1vzha04sy8qhg833xb829pqd1ar7kpdxfklzc30xbb6wcwgrqh0j";
            };
            xtermCSS = builtins.fetchurl {
              url = "https://cdn.jsdelivr.net/npm/xterm@4.8.1/css/xterm.css";
              sha256 = "sha256:070zqrzizm5kdkkrfv19rhg8q4v9kr4hrfr544im6h5w5hy3i1j0";
            };
            xtermAttachJS = builtins.fetchurl {
              url = "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.6.0/lib/xterm-addon-attach.min.js";
              sha256 = "sha256:1dpn6c8gc9xgq2xk7l0pikf59gw2h3c741p0hsiw4w3gysl93lkc";
            };
            xtermFitJS = builtins.fetchurl {
              url = "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.4.0/lib/xterm-addon-fit.min.js";
              sha256 = "sha256:1mpw2a2x96b26xfymz6prk4z41k45x9idfc7li48vam08d7x8rfn";
            };
          in
            ''
              mkdir -p static
              cp ${bulmaCSS}       static/bulma.css
              cp ${faviconICO}     static/favicon.ico
              cp ${jqueryJS}       static/jquery.js
              cp ${xtermJS}        static/xterm.js
              cp ${xtermCSS}       static/xterm.css
              cp ${xtermAttachJS}  static/xterm-attach.js
              cp ${xtermFitJS}     static/xterm-fit.js
            '';

        # The smos docs site
        #
        # We put these remote assets in place before the build so that they do not get fetched during the build
        packages.smos-docs-site.components.library.preConfigure =
          let
            bulmaCSS = builtins.fetchurl {
              url = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css";
              sha256 = "sha256:0lhpzahlszi5nr82n3sny5fjk4k1vaq11rdrddjmka23np53klqg";
            };
            fontawesomeCSS = builtins.fetchurl {
              url = "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css";
              sha256 = "sha256:1gch64hq7xc9jqvs7npsil2hwsigdjnvf78v1vpgswq3rhjyp6kr";
            };
            faviconICO = builtins.fetchurl {
              url = "https://cs-syd.eu/logo/res/favicon.ico";
              sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
            };
            asciinemaJS = builtins.fetchurl {
              url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js";
              sha256 = "sha256:092y2zl51z23jrl6mcqfxb64xaf9f2dx0j8kp69hp07m0935cz2p";
            };
            asciinemaCSS = builtins.fetchurl {
              url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css";
              sha256 = "sha256:1yi45fdps5mjqdwjhqwwzvlwxb4j7fb8451z7s6sdqmi7py8dksj";
            };
            # Copy the casts into the right place already so they don't have
            # to be cast during build.
            copyCastScript = name: cast: ''
              cp ${cast} content/casts/${name}.cast
            '';
            copyCasts = concatStringsSep "\n" (mapAttrsToList copyCastScript final.smosCasts);
          in
            ''
              mkdir -p static
              cp ${bulmaCSS}       static/bulma.css
              cp ${fontawesomeCSS} static/font-awesome.css
              cp ${faviconICO}     static/favicon.ico
              cp ${asciinemaJS}    static/asciinema-player.js
              cp ${asciinemaCSS}   static/asciinema-player.css

              mkdir -p content/casts
              ${copyCasts}
            '';

      }
      (import ./haskell-nix-modules/do-check-macos.nix)
      (import ./haskell-nix-modules/extra-ghc-flags.nix)
      (import ./haskell-nix-modules/clean-hpack.nix)
      (import ./haskell-nix-modules/static.nix)
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
              testOutput = final.haskell-nix.haskellLib.check test;
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
            rm -rf $out/{exactDep,envDep,lib,package.conf.d,share/doc,test-output} # Don't keep files we don't need.
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
  smosReleaseZip = final.stdenv.mkDerivation {
    name = "smos-release.zip";
    buildCommand = ''
      cd ${final.smosRelease}
      ${final.pkgs.zip}/bin/zip -r $out *
    '';
  };
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
    } // optionalAttrs (!isMacos) {
      "smos-docs-site" =
        let
          rawDocsSite = smosPkg "smos-docs-site";
          linkcheck = (
            import (
              builtins.fetchGit {
                url = "https://github.com/NorfairKing/linkcheck";
                rev = "dc65f22965d92e6145814cdc674d160f3c422559";
                ref = "master";
              }
            )
          ).linkcheck;
        in
          final.stdenv.mkDerivation {
            name = "smos-docs-site";
            buildCommand = ''
              mkdir -p $out
              ${final.xorg.lndir}/bin/lndir -silent ${rawDocsSite} $out

              $out/bin/smos-docs-site serve &
              sleep 1
              ${linkcheck}/bin/linkcheck http://localhost:8000
              ${final.killall}/bin/killall smos-docs-site
            '';
          };
    };

  smosCasts = import ./casts.nix final;
  ncurses = previous.ncurses.override { enableStatic = true; enableShared = true; };
}
