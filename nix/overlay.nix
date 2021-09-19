{ sources ? import ./sources.nix
, buildTools ? import sources.nixpkgs { }
}:
final: previous:

with final.lib;
let
  static = final.stdenv.hostPlatform.isMusl;
  isMacos = builtins.currentSystem == "x86_64-darwin";

  mergeListRecursively = final.callPackage ./merge-lists-recursively.nix { };
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
      castDerivation = name: final.mkCastDerivationFunction { pkgs = final // final.smosPackages; } {
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
              buildTools.haskellPackages.autoexporter
            ];
            doBenchmark = true;
            enableLibraryProfiling = false;
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
              "--ghc-options=-Wcompat"
              "--ghc-options=-Wno-compat-unqualified-imports"
            ];
            # Whatever is necessary for a static build.
            configureFlags = (old.configureFlags or [ ]) ++ optionals static [
              "--enable-executable-static"
              "--disable-executable-dynamic"
              "--ghc-option=-optl=-static"
              "--ghc-option=-static"
              "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${final.zlib.static}/lib"
              "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              "--extra-lib-dirs=${final.ncurses.override { enableStatic = true; }}/lib"
              "--extra-lib-dirs=${final.sqlite.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ];
            # Assert that the executables are indeed static
            postInstall = (old.postBuild or "") + optionalString static ''
              for exe in $out/bin/*
              do
                if ldd $exe
                then
                  echo "Not a static executable"
                  exit 1
                else
                  echo "Static executable: $exe"
                fi
              done
            '';
          }
        );
      smosPkg = name: buildStrictly (ownPkg name (final.gitignoreSource (../. + "/${name}")));
      smosPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (smosPkg name);
      smosPkgWithOwnComp = name: smosPkgWithComp name name;
      withLinksChecked = exeName: pkg:
        let
          linkcheck = (import sources.linkcheck).linkcheck;
          seocheck = (import sources.seocheck).seocheck;
        in
        final.stdenv.mkDerivation {
          name = "${exeName}-linkchecked";
          buildCommand = ''
            mkdir -p $out
            ${buildTools.xorg.lndir}/bin/lndir -silent ${pkg} $out

            $out/bin/${exeName} serve &
            sleep 1
            ${linkcheck}/bin/linkcheck http://localhost:8000 --check-fragments
            ${seocheck}/bin/seocheck http://localhost:8000
            ${buildTools.killall}/bin/killall ${exeName}
          '';
        };
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
          ${buildTools.sass}/bin/scss \
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
      generateOpenAPIClient = import (sources.openapi-code-generator + "/nix/generate-client.nix") { pkgs = buildTools; };
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
      stripe-client = ownPkg "stripe-client" generatedStripe.code;
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
      "smos-api" = smosPkg "smos-api";
      "smos-api-gen" = smosPkg "smos-api-gen";
      "smos-server" = smosPkgWithOwnComp "smos-server";
      "smos-server-gen" = smosPkg "smos-server-gen";
      "smos-client" = smosPkg "smos-client";
      "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
      "smos-sync-client-gen" = smosPkg "smos-sync-client-gen";
      "smos-shell" = smosPkg "smos-shell";
      "smos-github" = smosPkgWithOwnComp "smos-github";
      "smos-notify" = smosPkgWithOwnComp "smos-notify";
      inherit smos-web-style;
      inherit stripe-client;
    } // optionalAttrs (!isMacos) {
      # The 'thyme' dependency does not build on macos
      "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
      # smos-calendar-import is broken for me at the moment
      "smos-calendar-import" = smosPkgWithOwnComp "smos-calendar-import";
      inherit smos-web-server;
      inherit smos-docs-site;
    };

  smosRelease =
    final.symlinkJoin {
      name = "smos-release";
      paths = final.lib.attrValues final.smosPackages;
    };

  smosReleaseZip = final.stdenv.mkDerivation {
    name = "smos-release.zip";
    buildCommand = ''
      cd ${final.smosRelease}
      ${final.pkgs.zip}/bin/zip -r $out bin share/{zsh,bash-completion,mime,fish,applications}
    '';
  };

  moduleDocs =
    let
      eval = import (buildTools.path + "/nixos/lib/eval-config.nix") {
        pkgs = buildTools;
        modules = [
          (import ./nixos-module.nix {
            inherit sources;
            pkgs = buildTools;
            inherit (final) smosPackages;
            envname = "production";
          })
        ];
      };
    in
    (buildTools.nixosOptionsDoc {
      # options = filterRelevantOptions eval.options;
      options = eval.options;
    }).optionsJSON;


  # This can be deleted as soon as the following is in our nixpkgs:
  # https://github.com/NixOS/nixpkgs/pull/100838
  stripe-cli =
    final.stdenv.mkDerivation {
      name = "stripe-cli";
      src = builtins.fetchurl {
        url = "https://github.com/stripe/stripe-cli/releases/download/v1.5.12/stripe_1.5.12_linux_x86_64.tar.gz";
        sha256 = "sha256:077fx35phm2bjr147ycz77p76l3mx9vhaa1mx15kznw9y8jn6s14";
      };
      buildCommand = ''
        mkdir -p $out/bin
        tar xvzf $src --directory $out/bin
      '';
    };


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
              let
                passwordRepo = builtins.fetchGit {
                  url = "https://github.com/cdepillabout/password";
                  rev = "e90b7481af2d63de6b2d9ead3c03ddb798707d22";
                };
                passwordPkg = name: self.callCabal2nix name (passwordRepo + "/${name}") { };
                servantAuthRepo = builtins.fetchGit {
                  url = "https://github.com/haskell-servant/servant-auth";
                  rev = "296de3cb69135f83f0f01169fc10f8b3a2539405";
                };
                servantAuthPkg = name: dontCheck (self.callCabal2nix name (servantAuthRepo + "/${name}") { });
                servantAuthPackages = genAttrs [
                  "servant-auth"
                  "servant-auth-client"
                  "servant-auth-server"
                  "servant-auth-docs"
                  "servant-auth-swagger"
                ]
                  servantAuthPkg;
              in
              servantAuthPackages // {
                zip =
                    (
                    let super_zip =
                    self.callHackageDirect {
                      pkg = "zip";
                      ver = "1.7.1";
                      sha256 = "sha256:162ghfi8nvpvi6q8jav9zc5wjvxmf58kglbygr01pm75lyq59w8l";
                    } {};
                    in
                    dontCheck (enableCabalFlag super_zip "disable-bzip2")
                    );
                password = passwordPkg "password";
                password-types = passwordPkg "password-types";
                password-instances = passwordPkg "password-instances";
                iCalendar = self.callCabal2nix "iCalendar"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/NorfairKing/iCalendar";
                      rev = "70c924ad6275ba05a514e31af1607a5b175f98ad";
                    }
                  )
                  { };
                # vty = dontCheck (
                #   self.callCabal2nix "vty"
                #     (
                #       builtins.fetchGit {
                #         url = "https://github.com/jtdaugherty/vty";
                #         rev = "6a9c90da0e093cec1d4903924eb0f6a33be489cb";
                #       }
                #     )
                #     { }
                # );
                # vty = self.callHackageDirect {
                #   pkg = "vty";
                #   ver = "5.33";
                #   sha256 = "sha256:1x9bprikrgj386cwrhgx563j9plw9ap7akg7iqvlnhsfgrvypr0k";
                # } {};
                # vty = self.callHackage "vty" "5.33" {};
                ormode-parse = self.callCabal2nix "orgmode-parse"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/ixmatus/orgmode-parse";
                      rev = "1bdfbfe8fb7299724a6f6a122a93b2e96dd839f8";
                    }
                  )
                  { };
                haskeline = dontCheck (
                  self.callCabal2nix "haskeline"
                    (
                      builtins.fetchGit {
                        url = "https://github.com/NorfairKing/haskeline";
                        rev = "7c6491c55741608255c2681702381ce488692d15";
                      }
                    )
                    { }
                );
                template-haskell-reload = self.callCabal2nix "template-haskell-reload"
                  (
                    sources.template-haskell-reload
                    + "/template-haskell-reload"
                  )
                  { };
                yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                terminfo = self.callHackageDirect {
                  pkg = "terminfo";
                  ver = "0.4.1.5";
                  sha256 = "sha256:18a0aksk3x9akxz1jf0g2k9nx4i4ij5bdzq1qn0qj56i86qg4chr";
                } {};
                envparse = self.callHackage "envparse" "0.4.1" { };
                persistent-sqlite = if static then super.persistent-sqlite.override { sqlite = final.sqlite.overrideAttrs (old: { dontDisableStatic = true; }); } else super.persistent-sqlite;
                # These are turned off for the same reason as the local packages tests
                dirforest = if isMacos then dontCheck super.dirforest else super.dirforest;
                genvalidity-dirforest = if isMacos then dontCheck super.genvalidity-dirforest else super.genvalidity-dirforest;
                cursor-dirforest = if isMacos then dontCheck super.cursor-dirforest else super.cursor-dirforest;
              } // final.smosPackages
            );
      }
    );
}
