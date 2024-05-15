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

  smosDependencyGraph = final.makeDependencyGraph {
    name = "smos-dependency-graph";
    packages = builtins.attrNames final.smosReleasePackages;
    format = "svg";
    inherit (final) haskellPackages;
  };


  smosRelease = final.symlinkJoin {
    name = "smos-release";
    paths = attrValues final.smosReleasePackages;
    passthru = final.smosReleasePackages;
  };

  smosClientZipped = final.runCommand "release.zip" { } ''
    cd ${final.smosClientRelease}
    ${final.zip}/bin/zip $out -r .
  '';

  smosClientRelease = final.symlinkJoin {
    name = "smos-client-release";
    paths = attrValues final.smosClientPackages;
  };

  # Must not have the docs site
  # Should not have test suite executables
  smosClientPackages = builtins.removeAttrs final.smosReleasePackages [
    "smos-server-gen"
    "smos-docs-site"
  ];

  smosReleasePackages =
    let
      enableStatic = pkg:
        if final.stdenv.hostPlatform.isMusl
        then
          overrideCabal pkg
            (old:
              let
                # Until https://github.com/NixOS/nixpkgs/pull/311411
                terminfoDirs = final.lib.concatStringsSep ":" [
                  "/etc/terminfo" # Debian, Fedora, Gentoo
                  "/lib/terminfo" # Debian
                  "/usr/share/terminfo" # upstream default, probably all FHS-based distros
                  "/run/current-system/sw/share/terminfo" # NixOS
                ];
                staticNcurses = (
                  (final.ncurses.override {
                    enableStatic = true;
                  })
                ).overrideAttrs
                  (old: {
                    configureFlags = (old.configureFlags or [ ]) ++ [
                      "--with-terminfo-dirs=${terminfoDirs}"
                    ];
                  });
              in
              {
                configureFlags = (old.configureFlags or [ ]) ++ [
                  "--ghc-option=-optl=-static"
                  # Static
                  "--extra-lib-dirs=${final.gmp6.override { withStatic = true;
                }}/lib"
                  "--extra-lib-dirs=${final.zlib.static}/lib"
                  "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true;
                })}/lib"
                  # for -ltinfo
                  "--extra-lib-dirs=${staticNcurses}/lib"
                ];
                enableSharedExecutables = false;
                enableSharedLibraries = false;

                postInstall = (old.postInstall or "") + ''
                  for b in $out/bin/*
                  do
                    if ldd "$b"
                    then
                      echo "ldd succeeded on $b, which may mean that it is not statically linked"
                      exit 1
                    fi
                  done
                '';
              })
        else pkg;

    in
    mapAttrs
      (_: pkg: justStaticExecutables (enableStatic pkg))
      final.haskellPackages.smosPackages;

  nixosModuleDocs =
    let
      smos-module = import ./nixos-module.nix
        {
          inherit (final.smosReleasePackages) smos-docs-site smos-server smos-web-server;
          inherit (final.haskellPackages.looper) mkLooperOption;
        }
        {
          envname = "production";
        };
      eval = final.evalNixOSConfig {
        pkgs = final;
        modules = [
          smos-module
          { system.stateVersion = "23.11"; }
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
          { system.stateVersion = "23.11"; }
          smos-module
        ];
      };
    in
    (final.nixosOptionsDoc {
      options = eval.options;
    }).optionsJSON;

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
      casts = genAttrs castNames castDerivation;
    in
    final.linkFarm "smos-casts" casts;

  generatedSmosStripeCode = generatedStripe;

  sqlite =
    if final.stdenv.hostPlatform.isMusl
    then prev.sqlite.overrideAttrs (old: { dontDisableStatic = true; })
    else prev.sqlite;

  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super:
          let
            smosPackages =
              let
                ownPkg = name: src:
                  overrideCabal (self.callPackage src { }) (old: {
                    doBenchmark = true;
                    doHaddock = false;
                    doCoverage = false;
                    doHoogle = false;
                    doCheck = false; # Off by default, on for coverables
                    hyperlinkSource = false;
                    enableLibraryProfiling = false;
                    enableExecutableProfiling = false;
                    configureFlags = (old.configureFlags or [ ]) ++ [
                      # Optimisations
                      "--ghc-options=-O2"
                      # Extra warnings
                      "--ghc-options=-Wincomplete-uni-patterns"
                      "--ghc-options=-Wincomplete-record-updates"
                      "--ghc-options=-Wpartial-fields"
                      "--ghc-options=-Widentities"
                      "--ghc-options=-Wredundant-constraints"
                      "--ghc-options=-Wcpp-undef"
                      "--ghc-options=-Wunused-packages"
                      "--ghc-options=-Werror"
                    ];
                    # Ugly hack because we can't just add flags to the 'test' invocation.
                    # Show test output as we go, instead of all at once afterwards.
                    testTarget = (old.testTarget or "") + " --show-details=direct";
                  });
                smosPkg = name: buildStrictly (ownPkg name (../. + "/${name}"));
                smosPkgWithComp = exeName: name: self.generateOptparseApplicativeCompletions [ exeName ] (smosPkg name);
                smosPkgWithOwnComp = name: smosPkgWithComp name name;
                withTZTestData = pkg: (overrideCabal pkg) (old: {
                  testDepends = (old.testDepends or [ ]) ++ [
                    final.tzdata
                  ];
                });
                withLinksChecked = exeName: pkg:
                  overrideCabal pkg (old: {
                    postInstall = (old.postInstall or "") + ''
                      $out/bin/${exeName} &
                      sleep 1
                      ${final.linkcheck}/bin/linkcheck http://localhost:8080 --fetchers 2 --log-level Info --check-fragments
                      ${final.seocheck}/bin/seocheck http://localhost:8080   --fetchers 2 --log-level Info
                      ${final.killall}/bin/killall ${exeName}
                    '';
                  });
                withStaticResources = pkg: resources: overrideCabal pkg (old: {
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
                    (old.preConfigure or "") + ''
                      ${copyScript}
                    '';
                });

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
                  preConfigure = (old.preConfigure or "") + ''
                    export STYLE_FILE=${stylesheet}
                  '';
                });
                docs-site-pkg = overrideCabal (smosPkgWithOwnComp "smos-docs-site") (old: {
                  preConfigure = (old.preConfigure or "") + ''
                    export SMOS_DOCS_NIXOS_MODULE_DOCS="${final.nixosModuleDocs}/share/doc/nixos/options.json"
                    export SMOS_DOCS_HOME_MANAGER_MODULE_DOCS="${final.homeManagerModuleDocs}/share/doc/nixos/options.json"
                    export SMOS_DOCS_DEPENDENCY_GRAPH="${final.smosDependencyGraph}/smos-dependency-graph.svg"
                    export SMOS_DOCS_CASTS="${final.smosCasts}"

                    ln -s ${final.smosClientZipped} content/assets/smos-release.zip
                  '';
                });
                smos-docs-site = withLinksChecked "smos-docs-site" (withStaticResources docs-site-pkg {
                  "static/font-awesome.css" = builtins.fetchurl {
                    url = "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css";
                    sha256 = "sha256:1gch64hq7xc9jqvs7npsil2hwsigdjnvf78v1vpgswq3rhjyp6kr";
                  };
                  "static/favicon.ico" = builtins.fetchurl {
                    url = "https://cs-syd.eu/logo/res/favicon.ico";
                    sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
                  };
                  "static/asciinema-player.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.min.js";
                    sha256 = "sha256:0wcmqgi7054p8szamnhib0zwcpd1qrnfmclmbj6qwkkzc2i1fkvh";
                  };
                  "static/asciinema-player.css" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.css";
                    sha256 = "sha256:14s938lkzh250sdy9lwxjg0px7p8dx4sfc4c6p0zf1yiradc9dm2";
                  };
                });
                smos-web-server = withStaticResources (smosPkgWithOwnComp "smos-web-server") ({
                  "static/favicon.ico" = builtins.fetchurl {
                    url = "https://cs-syd.eu/logo/res/favicon.ico";
                    sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
                  };
                  "static/jquery.js" = builtins.fetchurl {
                    url = "https://code.jquery.com/jquery-3.7.1.min.js";
                    sha256 = "sha256:06hb7y19azzim1k53d1gw78fq6whw7s1qj7hpxf08sqz4kfr76pw";
                  };
                  "static/xterm.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js";
                    sha256 = "sha256:0pb5f3rjbpg7zz1zkd72vkwsma3pnj07mq46jj95yghy48dx67gw";
                  };
                  "static/xterm.css" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css";
                    sha256 = "sha256:0wj83vc6sp06lqkb2v972hq8fynca00rf17za51sshrvc0n3ybw3";
                  };
                  "static/xterm-attach.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.9.0/lib/xterm-addon-attach.min.js";
                    sha256 = "sha256:121ghlh0g5h3vqbh4z18yj1dag5xs20iy45rky88b0iarpndv5sr";
                  };
                  "static/xterm-fit.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/lib/xterm-addon-fit.min.js";
                    sha256 = "sha256:1qabf7dkj7q6bg31na0v2143iv4w9a6pjdml9brcsyj8qsamg3n7";
                  };
                  "static/asciinema-player.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.min.js";
                    sha256 = "sha256:0wcmqgi7054p8szamnhib0zwcpd1qrnfmclmbj6qwkkzc2i1fkvh";
                  };
                  "static/asciinema-player.css" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.css";
                    sha256 = "sha256:14s938lkzh250sdy9lwxjg0px7p8dx4sfc4c6p0zf1yiradc9dm2";
                  };
                  "static/bulma-carousel.js" = builtins.fetchurl {
                    url = "https://cdn.jsdelivr.net/npm/bulma-carousel@4.0.24/dist/js/bulma-carousel.min.js";
                    sha256 = "sha256:0cm7wj49qmbi9kp5hs3wc6vcr1h0d5h864pa5bc401nm5kppp958";
                  };
                } // mapAttrs' (name: value: nameValuePair "casts/${name}.cast" value) final.smosCasts);
                smos = overrideCabal (withTZTestData (smosPkgWithOwnComp "smos")) (old: {
                  postBuild = (old.postBuild or "") + ''
                    # Set up mime the types
                    mkdir -p $out/share/mime/packages
                    ln -s ${../mime/smos.mime-type} $out/share/mime/packages/smos.xml
                
                    # Set up the .desktop files
                    mkdir -p $out/share/applications
                    ln -s ${../mime/smos.desktop} $out/share/applications/smos.desktop
                  '';
                });
              in
              {
                inherit smos;
                "smos-e2e" = smosPkg "smos-e2e";
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
                "smos-query" = withTZTestData (smosPkgWithOwnComp "smos-query");
                "smos-single" = smosPkgWithOwnComp "smos-single";
                "smos-scheduler" = withTZTestData (smosPkgWithOwnComp "smos-scheduler");
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
                "smos-stripe-client" = self.callPackage final.generatedSmosStripeCode { };
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


            servantPkg = name: subdir:
              # Some tests are really slow so we turn them off.                         
              dontCheck (self.callCabal2nix name
                ((builtins.fetchGit {
                  url = "https://github.com/haskell-servant/servant";
                  rev = "552da96ff9a6d81a8553c6429843178d78356054";
                }) + "/${subdir}")
                { });
            servantPackages = {
              "servant" = servantPkg "servant" "servant";
              "servant-client" = servantPkg "servant-client" "servant-client";
              "servant-client-core" = servantPkg "servant-client-core" "servant-client-core";
              "servant-server" = servantPkg "servant-server" "servant-server";
              "servant-auth" = servantPkg "servant-auth-client" "servant-auth/servant-auth";
              "servant-auth-client" = servantPkg "servant-auth-client" "servant-auth/servant-auth-client";
              "servant-auth-server" = servantPkg "servant-auth-server" "servant-auth/servant-auth-server";
            };
            fixGHC = pkg:
              if final.stdenv.hostPlatform.isMusl
              then
                pkg.override
                  {
                    # To make sure that executables that need template
                    # haskell can be linked statically.
                    enableRelocatedStaticLibs = true;
                    enableShared = false;
                  }
              else pkg;
          in
          {
            # To override GHC, we need to override both `ghc` and the one in
            # `buildHaskellPackages` because otherwise this code in `generic-builder.nix`
            # will make our package depend on 2 different GHCs:
            #     nativeGhc = buildHaskellPackages.ghc;                                     
            #     depsBuildBuild = [ nativeGhc ] ...                                        
            #     nativeBuildInputs = [ ghc removeReferencesTo ] ...
            #                                
            #  See https://github.com/nh2/static-haskell-nix/blob/88f1e2d57e3f4cd6d980eb3d8f99d5e60040ad54/survey/default.nix#L1593        
            ghc = fixGHC super.ghc;
            buildHaskellPackages = old.buildHaskellPackages.override (oldBuildHaskellPackages: {
              ghc = fixGHC oldBuildHaskellPackages.ghc;
            });

            inherit smosPackages;
            zip = dontCheck (enableCabalFlag (super.zip.override { bzlib-conduit = null; }) "disable-bzip2");

            brick = self.callCabal2nix "brick"
              (builtins.fetchGit {
                url = "https://github.com/jtdaugherty/brick";
                rev = "fc6f5eed07829d3e3be6717097c0249b1f2a0c04";
              })
              { };
            vty = self.callCabal2nix "vty"
              (builtins.fetchGit {
                url = "https://github.com/jtdaugherty/vty";
                rev = "2f9eb83654f9942a4ec54d9d2335a941fa66e272";
              })
              { };
            vty-unix = self.callCabal2nix "vty-unix"
              (builtins.fetchGit {
                url = "https://github.com/jtdaugherty/vty-unix";
                rev = "f68bc3e6130d0df43494f315b37bb1410dbd7ce0";
              })
              { };
            vty-crossplatform = self.callCabal2nix "vty-crossplatform"
              (builtins.fetchGit {
                url = "https://github.com/jtdaugherty/vty-crossplatform";
                rev = "bc519c6724aeb7fcc27bc16700574fccf0e0128d";
              })
              { };

            # The test suite depends on postgres, which we don't need and fails
            # to build with static linking as-is.
            # If this doesn't work, we can also try to get postgres to build afteral:
            # https://github.com/nh2/static-haskell-nix/blob/88f1e2d57e3f4cd6d980eb3d8f99d5e60040ad54/survey/default.nix#L642
            esqueleto = dontCheck super.esqueleto;
            # These are turned off for the same reason as the local packages tests
          } // amazonkaPackages // servantPackages // smosPackages
      );
    }
    );
}
