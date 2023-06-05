final: prev:

with final.lib;
with final.haskell.lib;
let
  stripe-spec = builtins.fetchGit {
    url = "https://github.com/stripe/openapi";
    rev = "c48cf54aab65f4966ba285bdfaf86ed52f5fb70c";
  };

  generatedStripe = final.generateOpenAPIClient {
    name = "smos-stripe-client";
    configFile = ../stripe-client-gen.yaml;
    src = stripe-spec + "/openapi/spec3.yaml";
  };

in
{
  fcitx-engines = final.fcitx5;
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

  smosDependencyGraph = final.makeDependencyGraph {
    name = "smos-dependency-graph";
    packages = builtins.attrNames final.haskellPackages.smosPackages;
    format = "svg";
    inherit (final) haskellPackages;
  };

  smosReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables pkg)
    final.haskellPackages.smosPackages;

  smosRelease =
    final.symlinkJoin {
      name = "smos-release";
      paths = attrValues final.smosReleasePackages;
    };

  nixosModuleDocs =
    let
      smos-module = import ./nixos-module.nix
        {
          inherit (final.smosReleasePackages) smos-docs-site smos-server smos-web-server;
          inherit (final.haskellPackages) looper;
        }
        {
          envname = "production";
        };
      eval = final.evalNixOSConfig {
        pkgs = final;
        modules = [
          smos-module
          { system.stateVersion = "23.05"; }
        ];
      };
    in
    (final.nixosOptionsDoc {
      options = eval.options;
    }).optionsJSON;

  homeManagerModuleDocs =
    let
      smos-module = args@{ pkgs, config, lib, ... }: (import ./home-manager-module.nix) { inherit (final) smosReleasePackages; } (
        final.lib.recursiveUpdate args {
          config.xdg.dataHome = "/home/user/.local/share";
          config.home.homeDirectory = "/home/user";
        }
      );
      eval = final.evalNixOSConfig {
        pkgs = final;
        modules = [
          { config._module.check = false; }
          { system.stateVersion = "23.05"; }
          smos-module
        ];
      };
    in
    (final.nixosOptionsDoc {
      options = eval.options;
    }).optionsJSON;

  generatedSmosStripeCode = generatedStripe;

  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super:
          let
            smosPackages =
              let
                ownPkg = name: src:
                  overrideCabal (self.callPackage src { }) (old: {
                    buildDepends = (old.buildDepends or [ ]) ++ [
                      final.haskellPackages.autoexporter
                    ];
                    doBenchmark = true;
                    enableLibraryProfiling = false;
                    enableExecutableProfiling = false;
                    doCheck = false; # Off by default, on for coverables
                    buildFlags = (old.buildFlags or [ ]) ++ [
                      "--ghc-options=-O2"
                      "--ghc-options=-Wincomplete-uni-patterns"
                      "--ghc-options=-Wincomplete-record-updates"
                      "--ghc-options=-Wpartial-fields"
                      "--ghc-options=-Widentities"
                      "--ghc-options=-Wredundant-constraints"
                      "--ghc-options=-Wcpp-undef"
                      "--ghc-options=-Wunused-packages"
                    ];
                    # Ugly hack because we can't just add flags to the 'test' invocation.
                    # Show test output as we go, instead of all at once afterwards.
                    testTarget = (old.testTarget or "") + " --show-details=direct";
                  });
                smosPkg = name: buildStrictly (ownPkg name (../. + "/${name}"));
                smosPkgWithComp = exeName: name: self.generateOptparseApplicativeCompletions [ exeName ] (smosPkg name);
                smosPkgWithOwnComp = name: smosPkgWithComp name name;
                withTZData = pkg: (overrideCabal pkg) (old: {
                  testDepends = (old.testDepends or [ ]) ++ [
                    final.tzdata
                  ];
                });
                withLinksChecked = exeName: pkg:
                  overrideCabal pkg (old: {
                    postInstall = ''
                      ${old.postInstall or ""}

                      $out/bin/${exeName} &
                      sleep 1
                      ${final.linkcheck}/bin/linkcheck http://localhost:8080 --fetchers 2 --log-level Info --check-fragments
                      ${final.seocheck}/bin/seocheck http://localhost:8080   --fetchers 2 --log-level Info
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
                            ln -s ${resource} "''$path"
                          '';
                        copyScript = concatStringsSep "\n" (mapAttrsToList copyResource resources);
                      in
                      ''
                        ${old.preConfigure or ""}
                        ${copyScript}
                      '';
                  }
                );

                bulma = builtins.fetchGit {
                  url = "https://github.com/jgthms/bulma";
                  rev = "c02757cd3043a4b30231c72dd01cd735c3b3672c";
                };
                bulma-carousel = builtins.fetchGit {
                  url = "https://github.com/Wikiki/bulma-carousel";
                  rev = "71e38451f429af74aa8dd6c0d69ce9dd626f87f6";
                };
                bulma-pricingtable = builtins.fetchGit {
                  url = "https://github.com/Wikiki/bulma-pricingtable";
                  rev = "25ef9a4e97afd2da9bd92d3e1c83fbd0caf91102";
                };

                stylesheet = final.stdenv.mkDerivation {
                  name = "site-stylesheet.css";
                  src = ../smos-web-style/style/mybulma.scss;
                  buildCommand = ''
                    # Dependency submodules are fetched manually here
                    # so that we don't have to fetch the submodules of smos
                    # when importing smos from derivation.
                    ln -s ${bulma} bulma
                    ln -s ${bulma-carousel} bulma-carousel
                    ln -s ${bulma-pricingtable} bulma-pricingtable
    
                    # The file we want to compile
                    # We need to copy this so that the relative path within it resolves to here instead of wherever we would link it from.
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
                    export NIXOS_MODULE_DOCS="${final.nixosModuleDocs}/share/doc/nixos/options.json"
                    export HOME_MANAGER_MODULE_DOCS="${final.homeManagerModuleDocs}/share/doc/nixos/options.json"
                    export DEPENDENCY_GRAPH="${final.smosDependencyGraph}/smos-dependency-graph.svg"
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
                      ln -s ${../mime/smos.mime-type} $out/share/mime/packages/smos.xml
                
                      # Set up the .desktop files
                      mkdir -p $out/share/applications
                      ln -s ${../mime/smos.desktop} $out/share/applications/smos.desktop
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
                "smos-directory" = smosPkg "smos-directory";
                "smos-directory-gen" = smosPkg "smos-directory-gen";
                "smos-report" = smosPkg "smos-report";
                "smos-report-gen" = smosPkg "smos-report-gen";
                "smos-report-cursor" = smosPkg "smos-report-cursor";
                "smos-report-cursor-gen" = smosPkg "smos-report-cursor-gen";
                "smos-query" = withTZData (smosPkgWithOwnComp "smos-query");
                "smos-single" = smosPkgWithOwnComp "smos-single";
                "smos-scheduler" = withTZData (smosPkgWithOwnComp "smos-scheduler");
                "smos-archive" = smosPkgWithOwnComp "smos-archive";
                "smos-calendar-import" = smosPkgWithOwnComp "smos-calendar-import";
                "smos-api" = smosPkg "smos-api";
                "smos-api-gen" = smosPkg "smos-api-gen";
                "smos-server" = smosPkgWithOwnComp "smos-server";
                "smos-server-gen" = smosPkg "smos-server-gen";
                "smos-client" = smosPkg "smos-client";
                "smos-sync-client" = smosPkgWithOwnComp "smos-sync-client";
                "smos-sync-client-gen" = smosPkg "smos-sync-client-gen";
                "smos-cli" = smosPkg "smos-cli";
                "smos-github" = smosPkgWithOwnComp "smos-github";
                "smos-jobhunt" = smosPkgWithOwnComp "smos-jobhunt";
                "smos-notify" = smosPkgWithOwnComp "smos-notify";
                "smos-stripe-client" = self.callPackage (final.generatedSmosStripeCode + "/default.nix") { };
                inherit smos-web-style;
                inherit smos-web-server;
                inherit smos-docs-site;
              };
            amazonkaRepo = builtins.fetchGit {
              url = "https://github.com/brendanhay/amazonka";
              rev = "2dc498fe75ff47db2db3ee63e042b1aa3da57c0f";
            };
            amazonkaPkg = name: path: self.callCabal2nix name (amazonkaRepo + "/${path}") { };
            amazonkaPackages = builtins.mapAttrs amazonkaPkg {
              "amazonka" = "lib/amazonka";
              "amazonka-core" = "lib/amazonka-core";
              "amazonka-test" = "lib/amazonka-test";
              "amazonka-ses" = "lib/services/amazonka-ses";
              "amazonka-sso" = "lib/services/amazonka-sso";
              "amazonka-sts" = "lib/services/amazonka-sts";
            };


          in
          {
            inherit smosPackages;
            zip = dontCheck (enableCabalFlag (super.zip.override { bzlib-conduit = null; }) "disable-bzip2");
            # These are turned off for the same reason as the local packages tests
            brick = self.callCabal2nix "brick"
              (
                builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/brick-1.6/brick-1.6.tar.gz";
                  sha256 = "0smfwip8pyzl48l4y1p23fgbl063zdf3yzrbq41hb7lvsa7lbdiy";
                }
              )
              { };
            bimap = self.callCabal2nix "bimap"
              (
                builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/bimap-0.5.0/bimap-0.5.0.tar.gz";
                  sha256 = "1p1bqvkbzjkwhrhhwcx0d4j52pa7287jdh45c8xzgksh1z33xg55";
                }
              )
              { };
            text-zipper = self.callCabal2nix "text-zipper"
              (
                builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/text-zipper-0.13/text-zipper-0.13.tar.gz";
                  sha256 = "1wdr8bksdlzaqm2nnmj0nxlw6hkhgipwgb4c6aia4lk19h7vyvms";
                }
              )
              { };
            vty = self.callCabal2nix "vty"
              (
                builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/vty-5.36/vty-5.36.tar.gz";
                  sha256 = "05gnrp2qyc6199s9m2y28sxszv4h03y6nwf5j42vbgj2vn3k71cq";
                }
              )
              { };



          } // amazonkaPackages // smosPackages
      );
    }
    );
}
