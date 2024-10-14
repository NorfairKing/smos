{
  description = "smos";
  nixConfig = {
    extra-substituters = "https://smos.cachix.org";
    extra-trusted-public-keys = "smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    mergeful.url = "github:NorfairKing/mergeful";
    mergeful.flake = false;
    # When setting this back to normal, fix the overlay turning off tests too
    looper.url = "github:NorfairKing/looper?rev=6f3ed1854618090eacc2642c56984db8a8723f5e";
    looper.flake = false;
    cursor.url = "github:NorfairKing/cursor";
    cursor.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick";
    cursor-brick.flake = false;
    fuzzy-time.url = "github:NorfairKing/fuzzy-time";
    fuzzy-time.flake = false;
    cursor-fuzzy-time.url = "github:NorfairKing/cursor-fuzzy-time";
    cursor-fuzzy-time.flake = false;
    dirforest.url = "github:NorfairKing/dirforest";
    dirforest.flake = false;
    cursor-dirforest.url = "github:NorfairKing/cursor-dirforest";
    cursor-dirforest.flake = false;
    conformance.url = "github:NorfairKing/conformance";
    conformance.flake = false;
    ical.url = "github:NorfairKing/ical";
    ical.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote";
    yesod-static-remote.flake = false;
    template-haskell-reload.url = "github:NorfairKing/template-haskell-reload";
    template-haskell-reload.flake = false;
    openapi-code-generator.url = "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator";
    autorecorder.url = "github:NorfairKing/autorecorder";
    autorecorder.flake = false;
    linkcheck.url = "github:NorfairKing/linkcheck";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck";
    seocheck.flake = false;
    feedback.url = "github:NorfairKing/feedback";
    feedback.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
    get-flake.url = "github:ursi/get-flake";
    smos-latest-release.url = "github:NorfairKing/smos?ref=release";
    smos-latest-release.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , weeder-nix
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    , autodocodec
    , mergeful
    , looper
    , cursor
    , cursor-brick
    , fuzzy-time
    , cursor-fuzzy-time
    , dirforest
    , cursor-dirforest
    , conformance
    , ical
    , yesod-autoreload
    , yesod-static-remote
    , template-haskell-reload
    , openapi-code-generator
    , autorecorder
    , linkcheck
    , seocheck
    , feedback
    , dekking
    , get-flake
    , smos-latest-release
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (mergeful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (looper + "/nix/overlay.nix"))
          (import (cursor + "/nix/overlay.nix"))
          (import (cursor-brick + "/nix/overlay.nix"))
          (import (fuzzy-time + "/nix/overlay.nix"))
          (import (cursor-fuzzy-time + "/nix/overlay.nix"))
          (import (dirforest + "/nix/overlay.nix"))
          (import (cursor-dirforest + "/nix/overlay.nix"))
          (import (conformance + "/nix/overlay.nix"))
          (import (ical + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (template-haskell-reload + "/nix/overlay.nix"))
          (import (autorecorder + "/nix/overlay.nix"))
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (import (feedback + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
          (_:_: { makeDependencyGraph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph; })
          (_:_: { generateOpenAPIClient = openapi-code-generator.packages.${system}.default.passthru.generateOpenAPIClient; })
          (_:_: { evalNixOSConfig = args: import (nixpkgs + "/nixos/lib/eval-config.nix") (args // { inherit system; }); })
          self.overlays.${system}
        ];
      };
      pkgsMusl = pkgs.pkgsMusl;
      mkNixOSModule = import ./nix/nixos-module.nix {
        inherit (pkgsMusl.smosReleasePackages) smos-docs-site smos-server smos-web-server;
        inherit (pkgs.haskellPackages.looper) mkLooperOption;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = self.packages.${system}.dynamic;
        static = pkgsMusl.smosRelease;
        dynamic = pkgs.smosRelease;
      };
      apps.${system}.default = { type = "app"; program = "${pkgs.smosReleasePackages.smos}/bin/smos"; };
      checks.${system} =
        let
          mkE2ETest = import ./nix/e2e-test.nix {
            inherit (pkgs) nixosTest;
            inherit system get-flake;
            home-manager = home-manager.nixosModules.home-manager;
          };
        in
        {
          release = self.packages.${system}.default;
          static = self.packages.${system}.static;
          dynamic = self.packages.${system}.dynamic;
          shell = self.devShells.${system}.default;
          casts = pkgs.smosCasts;
          stylesheet = pkgs.smosStylesheet;
          nixosModuleDocs = pkgs.nixosModuleDocs;
          homeManagerModuleDocs = pkgs.homeManagerModuleDocs;
          generatedSmosStripeCode = pkgs.generatedSmosStripeCode;
          dependencyGraph = pkgs.smosDependencyGraph;
          e2e-test-current-compatibility = mkE2ETest {
            name = "current-compatibility";
            flakeUnderTest = self;
            flakeOverTest = self;
          };
          e2e-test-backward-compatibility = mkE2ETest {
            name = "backward-compatibility";
            flakeUnderTest = get-flake smos-latest-release;
            flakeOverTest = self;
          };
          e2e-test-forward-compatibility = mkE2ETest {
            name = "forward-compatibility";
            flakeUnderTest = self;
            flakeOverTest = get-flake smos-latest-release;
          };
          coverage-report = pkgs.dekking.makeCoverageReport {
            name = "test-coverage-report";
            packages = [
              "smos"
              "smos-api"
              "smos-archive"
              "smos-calendar-import"
              "smos-client"
              "smos-cursor"
              "smos-data"
              "smos-directory"
              "smos-github"
              "smos-jobhunt"
              "smos-notify"
              "smos-query"
              "smos-report"
              "smos-report-cursor"
              "smos-scheduler"
              "smos-server"
              "smos-single"
              # "smos-stripe-client" # No need for coverage for generated code
              "smos-sync-client"
              "smos-web-assets"
              "smos-web-server"
            ];
            # # No need for coverables for test packages
            coverage = [
              "smos-api-gen"
              "smos-cursor-gen"
              "smos-data-gen"
              "smos-directory-gen"
              "smos-report-cursor-gen"
              "smos-report-gen"
              "smos-server-gen"
              "smos-sync-client-gen"
              # Coverage for docs site is not interesting, but it runs parts of the rest
              "smos-docs-site"
            ];
          };
          weeder-check = pkgs.weeder-nix.makeWeederCheck {
            weederToml = ./weeder.toml;
            packages = builtins.attrNames pkgs.haskellPackages.smosPackages;
          };
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              ormolu.excludes = [
                "scripts/gen-changelog-release-section.hs"
                "scripts/make-release-tags.hs"
              ];
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "smos-shell";
        packages = p: builtins.attrValues p.smosPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
          hub
          pkgs.feedback
          pkgs.autorecorder
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook + pkgs.feedback.shellHook;

        SMOS_DOCS_NIXOS_MODULE_DOCS = "${pkgs.nixosModuleDocs}/share/doc/nixos/options.json";
        SMOS_DOCS_HOME_MANAGER_MODULE_DOCS = "${pkgs.homeManagerModuleDocs}/share/doc/nixos/options.json";
        SMOS_DOCS_DEPENDENCY_GRAPH = "${pkgs.smosDependencyGraph}/smos-dependency-graph.svg";
        SMOS_CASTS = "${pkgs.smosCasts}";
        SMOS_STYLE = "${pkgs.smosStylesheet}";
      };
      nixosModules.${system} = {
        e2eTest = self.nixosModuleFactories.${system}.e2eTest { envname = "production"; };

        default = self.nixosModules.${system}.dynamic;
        static = self.nixosModuleFactories.${system}.static { envname = "production"; };
        dynamic = self.nixosModuleFactories.${system}.dynamic { envname = "production"; };
      };
      nixosModuleFactories.${system} = {
        e2eTest = import ./nix/end-to-end-test-nixos-module.nix {
          inherit (pkgs.smosReleasePackages) smos-server-gen;
        };

        default = self.nixosModuleFactories.${system}.dynamic;
        static = import ./nix/nixos-module.nix {
          inherit (pkgsMusl.smosReleasePackages) smos-docs-site smos-server smos-web-server;
          inherit (pkgs.haskellPackages.looper) mkLooperOption;
        };
        dynamic = import ./nix/nixos-module.nix {
          inherit (pkgs.smosReleasePackages) smos-docs-site smos-server smos-web-server;
          inherit (pkgs.haskellPackages.looper) mkLooperOption;
        };
      };
      homeManagerModules.${system} = {
        default = self.homeManagerModules.${system}.dynamic;
        static = import ./nix/home-manager-module.nix { inherit (pkgsMusl) smosReleasePackages; };
        dynamic = import ./nix/home-manager-module.nix { inherit (pkgs) smosReleasePackages; };
      };
      nix-ci = {
        enable = true;
        auto-update = {
          enable = true;
          base = "development";
        };
        cachix = {
          name = "smos";
          public-key = "smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM=";
        };
      };
    };
}
