final: previous:

with final.lib;
let
  sources = import ./sources.nix;
  static = final.stdenv.hostPlatform.isMusl;
  isMacos = builtins.currentSystem == "x86_64-darwin";


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
      smosPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  overrideCabal (final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) { }) (
                    old: {
                      preCheck = ''
                        ${old.preCheck or ""}
                        export HSPEC_OPTIONS="--seed=42 --fail-fast"
                      '';
                      # Turn off certain suites on macos because they generate random
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
                  )
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
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
            ${final.xorg.lndir}/bin/lndir -silent ${pkg} $out

            $out/bin/${exeName} serve &
            sleep 1
            ${linkcheck}/bin/linkcheck http://localhost:8000
            ${seocheck}/bin/seocheck http://localhost:8000
            ${final.killall}/bin/killall ${exeName}
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
        buildInputs = [ final.sass ];
        buildCommand = ''
          # Dependency submodules are fetched manually here
          # so that we don't have to fetch the submodules of smos
          # when importing smos from derivation.
          ln -s ${sources.bulma} bulma
          ln -s ${sources.bulma-carousel} bulma-carousel
    
          # The file we want to compile
          # We need to copy this so that the relative path within it resolves to here instead of wherever we woudl link it from.
          cp $src mybulma.scss
          scss \
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

      smos-docs-site = withLinksChecked "smos-docs-site" (
        withStaticResources (smosPkgWithOwnComp "smos-docs-site") (
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
      "smos-shell" = smosPkg "smos-shell";
      "smos-github" = smosPkgWithOwnComp "smos-github";
      "smos-notify" = smosPkgWithOwnComp "smos-notify";
    } // optionalAttrs (!static) {
      # I couldn't get the stylesheet to build when building statically 
      inherit smos-web-style;
    } // optionalAttrs (!isMacos) {
      # The 'thyme' dependency does not build on macos
      "smos-convert-org" = smosPkgWithOwnComp "smos-convert-org";
    } // optionalAttrs (!static && !isMacos) {
      # I couldn't get the stylesheet to build when building statically 
      # and I couldn't get the casts to build when building on macos
      inherit smos-docs-site;
      inherit smos-web-server;
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
                  rev = "26434d4f6888faf8dc36425b20b59f0b5056d7f5";
                };
                passwordPkg = name: self.callCabal2nix name (passwordRepo + "/${name}") { };
                servantAuthRepo = builtins.fetchGit {
                  url = "https://github.com/haskell-servant/servant-auth";
                  rev = "296de3cb69135f83f0f01169fc10f8b3a2539405";
                };
                servantAuthPkg = name: self.callCabal2nix name (servantAuthRepo + "/${name}") { };
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
                password = passwordPkg "password";
                password-instances = passwordPkg "password-instances";
                iCalendar = self.callCabal2nix "iCalendar"
                  (
                    builtins.fetchGit {
                      url = "https://github.com/NorfairKing/iCalendar";
                      rev = "70c924ad6275ba05a514e31af1607a5b175f98ad";
                    }
                  )
                  { };
                vty = dontCheck (
                  self.callCabal2nix "vty"
                    (
                      builtins.fetchGit {
                        url = "https://github.com/jtdaugherty/vty";
                        rev = "6a9c90da0e093cec1d4903924eb0f6a33be489cb";
                      }
                    )
                    { }
                );
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
                terminfo = self.callHackage "terminfo" "0.4.1.4" { };
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
