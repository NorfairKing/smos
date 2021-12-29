{ sources ? import ./sources.nix
}:
final: previous:

with final.lib;
let
  isMacos = builtins.currentSystem == "x86_64-darwin";

  generateOpenAPIClient = import (sources.openapi-code-generator + "/nix/generate-client.nix") { pkgs = final; };
  generatedStripe = generateOpenAPIClient {
    name = "stripe-client";
    src = sources.stripe-spec + "/openapi/spec3.yaml";
    moduleName = "StripeClient";
    extraFlags = [
      "--property-type-suffix=\"'\""
      "--convert-to-camel-case"
    ];
    schemas = [
      "event"
      "checkout.session"
      "notification_event_data"
      "invoice"
      "subscription"
      "price"
      "customer"
    ];
    operations = [
      "GetEvents"
      "GetPricesPrice"
      "PostCustomers"
      "PostCheckoutSessions"
    ];
  };
  generatedStripeCode = generatedStripe.code;
in
{
  smosCasts =
    let
      castsDir = ../smos-docs-site/content/casts;
      castNames =
        builtins.map (removeSuffix ".yaml")
          (
            builtins.attrNames
              (
                filterAttrs
                  (p: v: v == "regular" && hasSuffix ".yaml" p)
                  (builtins.readDir castsDir)
              )
          );
      castDerivation = name: final.mkCastDerivationFunction { pkgs = final // final.smosReleasePackages; } {
        inherit name;
        src = ../smos-docs-site/content/casts + "/${name}.yaml";
        default-rows = 30;
        default-columns = 110;
        # debug = true;
      };
    in
    genAttrs castNames castDerivation;
  smosPackages = with final.haskell.lib;
    let
      ownPkg = name: src:
        overrideCabal (final.haskellPackages.callCabal2nixWithOptions name src "--no-hpack" { }) (
          old: {
            buildDepends = (old.buildDepends or [ ]) ++ [
              final.haskellPackages.autoexporter
            ];
            doBenchmark = true;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            # Turn off test suites on macos because they generate random
            # filepaths and that fails for some reason that I cannot investigate
            # because I don't own any apple products.
            doCheck = !isMacos;
            buildFlags = (old.buildFlags or [ ]) ++ [
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
            ];
          }
        );
      smosPkg = name: buildStrictly (ownPkg name (final.gitignoreSource (../. + "/${name}")));
      smosPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (smosPkg name);
      smosPkgWithOwnComp = name: smosPkgWithComp name name;
      withLinksChecked = exeName: pkg:
        let
          linkcheck = import sources.linkcheck;
          seocheck = import sources.seocheck;
        in
        overrideCabal pkg (old: {
          postInstall = ''
            ${old.postInstall or ""}

            $out/bin/${exeName} serve &
            sleep 1
            ${linkcheck}/bin/linkcheck http://localhost:8000 --check-fragments
            ${seocheck}/bin/seocheck http://localhost:8000
            ${final.killall}/bin/killall ${exeName}
          '';
        });
      withStaticResources = pkg: resources: overrideCabal pkg (
        old:
        {
          preConfigure =
            let
              copyResource = path: resource:
                ''
                  local path="${path}"
                  mkdir --parents $(dirname "''$path")
                  cp ${resource} "''$path"
                '';
              copyScript = concatStringsSep "\n" (mapAttrsToList copyResource resources);
            in
            ''
              ${old.preConfigure or ""}
              ${copyScript}
            '';
        }
      );

      stylesheet = final.stdenv.mkDerivation {
        name = "site-stylesheet.css";
        src = final.gitignoreSource ../smos-web-style/style/mybulma.scss;
        buildCommand = ''
          # Dependency submodules are fetched manually here
          # so that we don't have to fetch the submodules of smos
          # when importing smos from derivation.
          ln -s ${sources.bulma} bulma
          ln -s ${sources.bulma-carousel} bulma-carousel
          ln -s ${sources.bulma-pricingtable} bulma-pricingtable
    
          # The file we want to compile
          # We need to copy this so that the relative path within it resolves to here instead of wherever we woudl link it from.
          cp $src mybulma.scss
          ${final.sass}/bin/scss \
            --sourcemap=none \
            mybulma.scss:index.css --style compressed
          cp index.css $out
        '';
      };
      smos-web-style = overrideCabal (smosPkg "smos-web-style") (old: {
        preConfigure = ''
          ${old.preConfigure or ""}
          export STYLE_FILE=${stylesheet}
        '';
      });
      docs-site-pkg = overrideCabal (smosPkgWithOwnComp "smos-docs-site") (old: {
        preConfigure = ''
          ${old.preConfigure or ""}
          export MODULE_DOCS="${final.moduleDocs}/share/doc/nixos/options.json"
        '';
      });
      smos-docs-site = withLinksChecked "smos-docs-site" (
        withStaticResources docs-site-pkg (
          {
            "static/font-awesome.css" = builtins.fetchurl {
              url = "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css";
              sha256 = "sha256:1gch64hq7xc9jqvs7npsil2hwsigdjnvf78v1vpgswq3rhjyp6kr";
            };
            "static/favicon.ico" = builtins.fetchurl {
              url = "https://cs-syd.eu/logo/res/favicon.ico";
              sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
            };
            "static/asciinema-player.js" = builtins.fetchurl {
              url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js";
              sha256 = "sha256:092y2zl51z23jrl6mcqfxb64xaf9f2dx0j8kp69hp07m0935cz2p";
            };
            "static/asciinema-player.css" = builtins.fetchurl {
              url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css";
              sha256 = "sha256:1yi45fdps5mjqdwjhqwwzvlwxb4j7fb8451z7s6sdqmi7py8dksj";
            };
          } // mapAttrs' (name: value: nameValuePair "content/casts/${name}.cast" value) final.smosCasts
        )
      );
      smos-web-server = withStaticResources (smosPkgWithOwnComp "smos-web-server") ({
        "static/favicon.ico" = builtins.fetchurl {
          url = "https://cs-syd.eu/logo/res/favicon.ico";
          sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
        };
        "static/jquery.js" = builtins.fetchurl {
          url = "https://code.jquery.com/jquery-3.3.1.min.js";
          sha256 = "sha256:1vq2bp290rhby5l09dv5khqwv3ysnzbddggbgk6m4hl9y9pl42hn";
        };
        "static/xterm.js" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/xterm@4.8.1/lib/xterm.min.js";
          sha256 = "sha256:1vzha04sy8qhg833xb829pqd1ar7kpdxfklzc30xbb6wcwgrqh0j";
        };
        "static/xterm.css" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/xterm@4.8.1/css/xterm.css";
          sha256 = "sha256:070zqrzizm5kdkkrfv19rhg8q4v9kr4hrfr544im6h5w5hy3i1j0";
        };
        "static/xterm-attach.js" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.6.0/lib/xterm-addon-attach.min.js";
          sha256 = "sha256:1dpn6c8gc9xgq2xk7l0pikf59gw2h3c741p0hsiw4w3gysl93lkc";
        };
        "static/xterm-fit.js" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.4.0/lib/xterm-addon-fit.min.js";
          sha256 = "sha256:1mpw2a2x96b26xfymz6prk4z41k45x9idfc7li48vam08d7x8rfn";
        };
        "static/asciinema-player.js" = builtins.fetchurl {
          url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js";
          sha256 = "sha256:092y2zl51z23jrl6mcqfxb64xaf9f2dx0j8kp69hp07m0935cz2p";
        };
        "static/asciinema-player.css" = builtins.fetchurl {
          url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css";
          sha256 = "sha256:1yi45fdps5mjqdwjhqwwzvlwxb4j7fb8451z7s6sdqmi7py8dksj";
        };
        "static/bulma-carousel.js" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/bulma-carousel@4.0.3/dist/js/bulma-carousel.min.js";
          sha256 = "sha256:0cm7wj49qmbi9kp5hs3wc6vcr1h0d5h864pa5bc401nm5kppp958";
        };
      } // mapAttrs' (name: value: nameValuePair "casts/${name}.cast" value) final.smosCasts);
      smos = overrideCabal (smosPkgWithOwnComp "smos") (
        old: {
          postBuild = ''
            ${old.postBuild or ""}
            # Set up mime the types
            mkdir -p $out/share/mime/packages
            cp ${../mime/smos.mime-type} $out/share/mime/packages/smos.xml
                
            # Set up the .desktop files
            mkdir -p $out/share/applications
            cp ${../mime/smos.desktop} $out/share/applications/smos.desktop
          '';
        }
      );
    in
    {
      inherit smos;
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
      "smos-calendar-import" = smosPkgWithOwnComp "smos-calendar-import";
      "smos-api" = smosPkg "smos-api";
      "smos-api-gen" = smosPkg "smos-api-gen";
      "smos-server" = smosPkgWithOwnComp "smos-server";
      "smos-server-gen" = smosPkg "smos-server-gen";
      "smos-client" = smosPkg "smos-client";
      "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
      "smos-sync-client-gen" = smosPkg "smos-sync-client-gen";
      "smos-github" = smosPkgWithOwnComp "smos-github";
      "smos-notify" = smosPkgWithOwnComp "smos-notify";
      inherit smos-web-style;
    } // optionalAttrs (!isMacos) {
      # The 'thyme' dependency does not build on macos
      "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
      inherit smos-web-server;
      inherit smos-docs-site;
    };

  smosReleasePackages = mapAttrs (_: pkg: final.haskell.lib.justStaticExecutables pkg) final.smosPackages;

  smosRelease =
    final.symlinkJoin {
      name = "smos-release";
      paths = final.lib.attrValues final.smosReleasePackages;
    };

  moduleDocs =
    let
      eval = import (final.path + "/nixos/lib/eval-config.nix") {
        pkgs = final;
        modules = [
          (import ./nixos-module.nix {
            inherit sources;
            pkgs = final;
            inherit (final) smosReleasePackages;
            envname = "production";
          })
        ];
      };
    in
    (final.nixosOptionsDoc {
      options = eval.options;
    }).optionsJSON;

  inherit generatedStripeCode;

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super: with final.haskell.lib;
              {
                zip = dontCheck (enableCabalFlag (super.zip.override { bzlib-conduit = null; }) "disable-bzip2");
                iCalendar = self.callCabal2nix "iCalendar"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/NorfairKing/iCalendar";
                      rev = "e08c16dceaab4d15b0f00860512018bc64791f07";
                    }
                  )
                  { };
                ormode-parse = self.callCabal2nix "orgmode-parse"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/ixmatus/orgmode-parse";
                      rev = "1bdfbfe8fb7299724a6f6a122a93b2e96dd839f8";
                    }
                  )
                  { };
                template-haskell-reload = self.callCabal2nix "template-haskell-reload"
                  (
                    sources.template-haskell-reload
                    + "/template-haskell-reload"
                  )
                  { };
                yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                # These are turned off for the same reason as the local packages tests
                dirforest = if isMacos then dontCheck super.dirforest else super.dirforest;
                genvalidity-dirforest = if isMacos then dontCheck super.genvalidity-dirforest else super.genvalidity-dirforest;
                cursor-dirforest = if isMacos then dontCheck super.cursor-dirforest else super.cursor-dirforest;
                stripe-client = generatedStripe.package;
              } // final.smosPackages
            );
      }
    );
}
