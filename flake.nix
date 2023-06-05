{
  description = "smos";
  nixConfig = {
    extra-substituters = "https://smos.cachix.org";
    extra-trusted-public-keys = "smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    mergeful.url = "github:NorfairKing/mergeful";
    mergeful.flake = false;
    looper.url = "github:NorfairKing/looper";
    looper.flake = false;
    cursor.url = "github:NorfairKing/cursor";
    cursor.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick?ref=flake";
    cursor-brick.flake = false;
    fuzzy-time.url = "github:NorfairKing/fuzzy-time?ref=flake";
    fuzzy-time.flake = false;
    cursor-fuzzy-time.url = "github:NorfairKing/cursor-fuzzy-time?ref=flake";
    cursor-fuzzy-time.flake = false;
    dirforest.url = "github:NorfairKing/dirforest";
    dirforest.flake = false;
    cursor-dirforest.url = "github:NorfairKing/cursor-dirforest?ref=flake";
    cursor-dirforest.flake = false;
    ical.url = "github:NorfairKing/ical";
    ical.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote";
    yesod-static-remote.flake = false;
    template-haskell-reload.url = "github:NorfairKing/template-haskell-reload";
    template-haskell-reload.flake = false;
    openapi-code-generator.url = "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator?ref=flake";
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
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , mergeful
    , looper
    , cursor
    , cursor-brick
    , fuzzy-time
    , cursor-fuzzy-time
    , dirforest
    , cursor-dirforest
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
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (mergeful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (looper + "/nix/overlay.nix"))
          (import (cursor + "/nix/overlay.nix"))
          (import (cursor-brick + "/nix/overlay.nix"))
          (import (fuzzy-time + "/nix/overlay.nix"))
          (import (cursor-fuzzy-time + "/nix/overlay.nix"))
          (import (dirforest + "/nix/overlay.nix"))
          (import (cursor-dirforest + "/nix/overlay.nix"))
          (import (ical + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (template-haskell-reload + "/nix/overlay.nix"))
          (import (openapi-code-generator + "/nix/overlay.nix"))
          (import (autorecorder + "/nix/overlay.nix"))
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (import (feedback + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (_:_: { makeDependencyGraph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph; })
          (_:_: { generateOpenAPIClient = openapi-code-generator.packages.${system}.default.passthru.generateOpenAPIClient; })
          (_:_: { evalNixOSConfig = args: import (nixpkgs + "/nixos/lib/eval-config.nix") (args // { inherit system; }); })
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkE2ETestNixOSModule = import ./nix/end-to-end-test-nixos-module.nix {
        inherit (pkgs.smosReleasePackages) smos-server-gen;
      };
      mkNixOSModule = import ./nix/nixos-module.nix {
        inherit (pkgs.smosReleasePackages) smos-docs-site smos-server smos-web-server;
        inherit (pkgs.haskellPackages) looper;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.smosRelease;
        nixosModuleDocs = pkgs.nixosModuleDocs;
        homeManagerModuleDocs = pkgs.homeManagerModuleDocs;
        generatedSmosStripeCode = pkgs.generatedSmosStripeCode;
        dependencyGraph = pkgs.smosDependencyGraph;
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
          shell = self.devShells.${system}.default;
          e2e-test-current-compatibility = mkE2ETest {
            name = "current-compatibility";
            flakeUnderTest = self;
            flakeOverTest = self;
          };
          e2e-test-backward-compatibility = mkE2ETest {
            name = "backward-compatibility";
            flakeUnderTest = self;
            flakeOverTest = get-flake smos-latest-release;
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
              "smos-web-server"
              "smos-web-style"
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
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
          sass
          hub
          pkgs.feedback
          pkgs.autorecorder
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook + pkgs.feedback.shellHook;
      };
      nixosModules.${system} = {
        default = mkNixOSModule { envname = "production"; };
        e2eTest = mkE2ETestNixOSModule { envname = "production"; };
      };
      nixosModuleFactories.${system} = {
        default = mkNixOSModule;
        e2eTest = mkE2ETestNixOSModule;
      };
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { smosReleasePackages = pkgs.smosReleasePackages; };
    };
}
