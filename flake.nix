{
  description = "Smos";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;

    # Dependencies extracted from stack.yaml
    org-parser = {
      url = github:lucasvreis/org-parser/c81233122812d795087b313e100c759af8709e31;
      flake = false;
    };

    iCalendar = {
      url = github:NorfairKing/iCalendar/e08c16dceaab4d15b0f00860512018bc64791f07;
      flake = false;
    };

    typed-uuid = {
      url = github:NorfairKing/typed-uuid/00fbc7e0380ab2ff72e8fd02323e76f13b7d5b59;
      flake = false;
    };

    mergeful = {
      url = github:NorfairKing/mergeful/3b6beea95dcb8ce3db9c11fec0481cd10123476c;
      flake = false;
    };

    looper = {
      url = github:NorfairKing/looper/6f7d44f563a7e21c3ca5fa24ebe01ed1665426ca;
      flake = false;
    };

    pretty-relative-time = {
      url = github:NorfairKing/pretty-relative-time/a634358ff274380a12360f7814c3aea46ea35b1b;
      flake = false;
    };

    cursor-fuzzy-time = {
      url = github:NorfairKing/cursor-fuzzy-time/86830e3c14e1ec054e4423742eb34d1c49f9b8b0;
      flake = false;
    };

    fuzzy-time = {
      url = github:NorfairKing/fuzzy-time/af42de90fd04d8506a440f439c6628c64d33b7d2;
      flake = false;
    };

    dirforest = {
      url = github:NorfairKing/dirforest/69e8ae036b047fae105c1fe990e175a7572a3eba;
      flake = false;
    };

    cursor-dirforest = {
      url = github:NorfairKing/cursor-dirforest/6ad5b168e26eb4e647df9f007d812aaf59338d40;
      flake = false;
    };

    cursor-brick = {
      url = github:NorfairKing/cursor-brick/5c1d1306632403a3dc11ddeda10deee932c0b307;
      flake = false;
    };

    cursor = {
      url = github:NorfairKing/cursor/5f18d58d1b34a752d24a94590c2cd35e8b6d557b;
      flake = false;
    };

    autodocodec = {
      url = github:NorfairKing/autodocodec/a60fbd4db121c0529f9ceb68c58c289daa693db2;
      flake = false;
    };

    safe-coloured-text = {
      url = github:NorfairKing/safe-coloured-text/d1a727998fa58ecf38022906b552d33e57e3f308;
      flake = false;
    };

    sydtest = {
      url = github:NorfairKing/sydtest/cf1bb414f0c0ce7bbf64659da94b8cd890c9e536;
      flake = false;
    };

    validity = {
      url = github:NorfairKing/validity/d88be911a7e2a84f6c089e9269aaed8d10a74acd;
      flake = false;
    };

    yesod-static-remote = {
      url = github:NorfairKing/yesod-static-remote/ed6bf8ef434d49b160429028613a1f6882fccfdf;
      flake = false;
    };

    yesod-autoreload = {
      url = github:NorfairKing/yesod-autoreload/7135e864c0d4a48efeae473ee2761f5168946e58;
      flake = false;
    };

    template-haskell-reload = {
      url = github:NorfairKing/template-haskell-reload/c416550db3f353bad65980a8ecd9b3b81fa504bd;
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , org-parser
    , iCalendar
    , typed-uuid
    , mergeful
    , looper
    , pretty-relative-time
    , cursor-fuzzy-time
    , fuzzy-time
    , dirforest
    , cursor-dirforest
    , cursor-brick
    , cursor
    , autodocodec
    , safe-coloured-text
    , sydtest
    , validity
    , yesod-static-remote
    , yesod-autoreload
    , template-haskell-reload
    }:

    flake-utils.lib.eachDefaultSystem (
      system:

      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc902.extend (final: prev:
          let
            addBuildDeps = deps: pkg: pkg.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ deps;
            });

            disableTestsOnMac = if pkgs.stdenv.isDarwin then pkgs.haskell.lib.dontCheck else x: x;
          in
          {
            # Dependencies
            autodocodec =
              final.callCabal2nix
                "autodocodec"
                (autodocodec + /autodocodec)
                { };

            autodocodec-schema =
              final.callCabal2nix
                "autodocodec-schema"
                (autodocodec + /autodocodec-schema)
                { };

            autodocodec-yaml =
              final.callCabal2nix
                "autodocodec-yaml"
                (autodocodec + /autodocodec-yaml)
                { };

            cursor =
              final.callCabal2nix
                "cursor"
                (cursor + /cursor)
                { };

            cursor-brick =
              final.callCabal2nix
                "cursor-brick"
                (cursor-brick + /cursor-brick)
                { };

            cursor-dirforest =
              final.callCabal2nix
                "cursor-dirforest"
                (cursor-dirforest + /cursor-dirforest)
                { };

            cursor-dirforest-brick =
              final.callCabal2nix
                "cursor-dirforest-brick"
                (cursor-dirforest + /cursor-dirforest-brick)
                { };

            cursor-dirforest-gen =
              final.callCabal2nix
                "cursor-dirforest-gen"
                (cursor-dirforest + /cursor-dirforest-gen)
                { };

            cursor-fuzzy-time =
              final.callCabal2nix
                "cursor-fuzzy-time"
                (cursor-fuzzy-time + /cursor-fuzzy-time)
                { };

            cursor-fuzzy-time-gen =
              final.callCabal2nix
                "cursor-fuzzy-time-gen"
                (cursor-fuzzy-time + /cursor-fuzzy-time-gen)
                { };

            cursor-gen =
              final.callCabal2nix
                "cursor-gen"
                (cursor + /cursor-gen)
                { };

            dirforest =
              final.callCabal2nix
                "dirforest"
                (dirforest + /dirforest)
                { };

            fuzzy-time =
              final.callCabal2nix
                "fuzzy-time"
                (fuzzy-time + /fuzzy-time)
                { };

            fuzzy-time-gen =
              final.callCabal2nix
                "fuzzy-time-gen"
                (fuzzy-time + /fuzzy-time-gen)
                { };

            genvalidity =
              final.callCabal2nix
                "genvalidity"
                (validity + /genvalidity)
                { };

            genvalidity-aeson =
              final.callCabal2nix
                "genvalidity-aeson"
                (validity + /genvalidity-aeson)
                { };

            genvalidity-bytestring =
              final.callCabal2nix
                "genvalidity-bytestring"
                (validity + /genvalidity-bytestring)
                { };

            genvalidity-containers =
              final.callCabal2nix
                "genvalidity-containers"
                (validity + /genvalidity-containers)
                { };

            genvalidity-criterion =
              final.callCabal2nix
                "genvalidity-criterion"
                (validity + /genvalidity-criterion)
                { };

            genvalidity-dirforest = disableTestsOnMac (
              final.callCabal2nix
                "genvalidity-dirforest"
                (dirforest + /genvalidity-dirforest)
                { }
            );

            genvalidity-hspec =
              final.callCabal2nix
                "genvalidity-hspec"
                (validity + /genvalidity-hspec)
                { };

            genvalidity-hspec-aeson =
              final.callCabal2nix
                "genvalidity-hspec-aeson"
                (validity + /genvalidity-hspec-aeson)
                { };

            genvalidity-hspec-binary =
              final.callCabal2nix
                "genvalidity-hspec-binary"
                (validity + /genvalidity-hspec-binary)
                { };

            genvalidity-hspec-cereal =
              final.callCabal2nix
                "genvalidity-hspec-cereal"
                (validity + /genvalidity-hspec-cereal)
                { };

            genvalidity-hspec-hashable =
              final.callCabal2nix
                "genvalidity-hspec-hashable"
                (validity + /genvalidity-hspec-hashable)
                { };

            genvalidity-hspec-optics =
              final.callCabal2nix
                "genvalidity-hspec-optics"
                (validity + /genvalidity-hspec-optics)
                { };

            genvalidity-hspec-persistent =
              final.callCabal2nix
                "genvalidity-hspec-persistent"
                (validity + /genvalidity-hspec-persistent)
                { };

            genvalidity-mergeful =
              final.callCabal2nix
                "genvalidity-mergeful"
                (mergeful + /genvalidity-mergeful)
                { };

            genvalidity-path =
              final.callCabal2nix
                "genvalidity-path"
                (validity + /genvalidity-path)
                { };

            genvalidity-persistent =
              final.callCabal2nix
                "genvalidity-persistent"
                (validity + /genvalidity-persistent)
                { };

            genvalidity-property =
              final.callCabal2nix
                "genvalidity-property"
                (validity + /genvalidity-property)
                { };

            genvalidity-scientific =
              final.callCabal2nix
                "genvalidity-scientific"
                (validity + /genvalidity-scientific)
                { };

            genvalidity-sydtest =
              final.callCabal2nix
                "genvalidity-sydtest"
                (validity + /genvalidity-sydtest)
                { };

            genvalidity-sydtest-aeson =
              final.callCabal2nix
                "genvalidity-sydtest-aeson"
                (validity + /genvalidity-sydtest-aeson)
                { };

            genvalidity-sydtest-hashable =
              final.callCabal2nix
                "genvalidity-sydtest-hashable"
                (validity + /genvalidity-sydtest-hashable)
                { };

            genvalidity-sydtest-lens =
              final.callCabal2nix
                "genvalidity-sydtest-lens"
                (validity + /genvalidity-sydtest-lens)
                { };

            genvalidity-sydtest-persistent =
              final.callCabal2nix
                "genvalidity-sydtest-persistent"
                (validity + /genvalidity-sydtest-persistent)
                { };

            genvalidity-text =
              final.callCabal2nix
                "genvalidity-text"
                (validity + /genvalidity-text)
                { };

            genvalidity-time =
              final.callCabal2nix
                "genvalidity-time"
                (validity + /genvalidity-time)
                { };

            genvalidity-unordered-containers =
              final.callCabal2nix
                "genvalidity-unordered-containers"
                (validity + /genvalidity-unordered-containers)
                { };

            genvalidity-uuid =
              final.callCabal2nix
                "genvalidity-uuid"
                (validity + /genvalidity-uuid)
                { };

            genvalidity-vector =
              final.callCabal2nix
                "genvalidity-vector"
                (validity + /genvalidity-vector)
                { };

            iCalendar =
              final.callCabal2nix
                "iCalendar"
                iCalendar
                { };

            looper =
              final.callCabal2nix
                "looper"
                (looper + /looper)
                { };

            mergeful =
              final.callCabal2nix
                "mergeful"
                (mergeful + /mergeful)
                { };

            mergeful-persistent =
              final.callCabal2nix
                "mergeful-persistent"
                (mergeful + /mergeful-persistent)
                { };

            nvalidity-typed-uuid =
              final.callCabal2nix
                "genvalidity-typed-uuid"
                (typed-uuid + /nvalidity-typed-uuid)
                { };

            org-parser =
              final.callCabal2nix
                "org-parser"
                org-parser
                { };

            ped-uuid =
              final.callCabal2nix
                "typed-uuid"
                (typed-uuid + /ped-uuid)
                { };

            pretty-relative-time =
              final.callCabal2nix
                "pretty-relative-time"
                pretty-relative-time
                { };

            safe-coloured-text =
              final.callCabal2nix
                "safe-coloured-text"
                (safe-coloured-text + /safe-coloured-text)
                { };

            safe-coloured-text-gen =
              final.callCabal2nix
                "safe-coloured-text-gen"
                (safe-coloured-text + /safe-coloured-text-gen)
                { };

            safe-coloured-text-layout =
              final.callCabal2nix
                "safe-coloured-text-layout"
                (safe-coloured-text + /safe-coloured-text-layout)
                { };

            safe-coloured-text-layout-gen =
              final.callCabal2nix
                "safe-coloured-text-layout-gen"
                (safe-coloured-text + /safe-coloured-text-layout-gen)
                { };

            safe-coloured-text-terminfo =
              final.callCabal2nix
                "safe-coloured-text-terminfo"
                (safe-coloured-text + /safe-coloured-text-terminfo)
                { };

            sydtest =
              final.callCabal2nix
                "sydtest"
                (sydtest + /sydtest)
                { };

            sydtest-aeson =
              final.callCabal2nix
                "sydtest-aeson"
                (sydtest + /sydtest-aeson)
                { };

            sydtest-discover =
              final.callCabal2nix
                "sydtest-discover"
                (sydtest + /sydtest-discover)
                { };

            sydtest-persistent =
              final.callCabal2nix
                "sydtest-persistent"
                (sydtest + /sydtest-persistent)
                { };

            sydtest-persistent-sqlite =
              final.callCabal2nix
                "sydtest-persistent-sqlite"
                (sydtest + /sydtest-persistent-sqlite)
                { };

            sydtest-servant =
              final.callCabal2nix
                "sydtest-servant"
                (sydtest + /sydtest-servant)
                { };

            sydtest-wai =
              final.callCabal2nix
                "sydtest-wai"
                (sydtest + /sydtest-wai)
                { };

            sydtest-yesod =
              final.callCabal2nix
                "sydtest-yesod"
                (sydtest + /sydtest-yesod)
                { };

            template-haskell-reload =
              final.callCabal2nix
                "template-haskell-reload"
                (template-haskell-reload + /template-haskell-reload)
                { };

            validity =
              final.callCabal2nix
                "validity"
                (validity + /validity)
                { };

            validity-aeson =
              final.callCabal2nix
                "validity-aeson"
                (validity + /validity-aeson)
                { };

            validity-bytestring =
              final.callCabal2nix
                "validity-bytestring"
                (validity + /validity-bytestring)
                { };

            validity-containers =
              final.callCabal2nix
                "validity-containers"
                (validity + /validity-containers)
                { };

            validity-path =
              final.callCabal2nix
                "validity-path"
                (validity + /validity-path)
                { };

            validity-persistent =
              final.callCabal2nix
                "validity-persistent"
                (validity + /validity-persistent)
                { };

            validity-primitive =
              final.callCabal2nix
                "validity-primitive"
                (validity + /validity-primitive)
                { };

            validity-scientific =
              final.callCabal2nix
                "validity-scientific"
                (validity + /validity-scientific)
                { };

            validity-text =
              final.callCabal2nix
                "validity-text"
                (validity + /validity-text)
                { };

            validity-time =
              final.callCabal2nix
                "validity-time"
                (validity + /validity-time)
                { };

            validity-unordered-containers =
              final.callCabal2nix
                "validity-unordered-containers"
                (validity + /validity-unordered-containers)
                { };

            validity-uuid =
              final.callCabal2nix
                "validity-uuid"
                (validity + /validity-uuid)
                { };

            validity-vector =
              final.callCabal2nix
                "validity-vector"
                (validity + /validity-vector)
                { };

            yesod-autoreload =
              final.callCabal2nix
                "yesod-autoreload"
                yesod-autoreload
                { };

            yesod-static-remote =
              final.callCabal2nix
                "yesod-static-remote"
                yesod-static-remote
                { };

            # Smos packages
            smos = disableTestsOnMac (
              final.callCabal2nix
                "smos" ./smos
                { }
            );

            smos-api =
              final.callCabal2nix
                "smos-api" ./smos-api
                { };

            smos-api-gen =
              final.callCabal2nix
                "smos-api-gen" ./smos-api-gen
                { };

            smos-archive = disableTestsOnMac (
              addBuildDeps [ final.autoexporter ] (
                final.callCabal2nix
                  "smos-archive" ./smos-archive
                  { }
              )
            );

            smos-calendar-import =
              final.callCabal2nix
                "smos-calendar-import" ./smos-calendar-import
                { };

            smos-client =
              final.callCabal2nix
                "smos-client" ./smos-client
                { };

            smos-cursor =
              final.callCabal2nix
                "smos-cursor" ./smos-cursor
                { };

            smos-cursor-gen = disableTestsOnMac (
              final.callCabal2nix
                "smos-cursor-gen" ./smos-cursor-gen
                { }
            );

            smos-data =
              final.callCabal2nix
                "smos-data" ./smos-data
                { };

            smos-data-gen =
              final.callCabal2nix
                "smos-data-gen" ./smos-data-gen
                { };

            smos-github =
              final.callCabal2nix
                "smos-github" ./smos-github
                { };

            smos-notify =
              final.callCabal2nix
                "smos-notify" ./smos-notify
                { };

            smos-query =
              final.callCabal2nix
                "smos-query" ./smos-query
                { };

            smos-report =
              final.callCabal2nix
                "smos-report" ./smos-report
                { };

            smos-report-cursor =
              final.callCabal2nix
                "smos-report-cursor" ./smos-report-cursor
                { };

            smos-report-cursor-gen =
              final.callCabal2nix
                "smos-report-cursor-gen" ./smos-report-cursor-gen
                { };

            smos-report-gen = disableTestsOnMac (
              final.callCabal2nix
                "smos-report-gen" ./smos-report-gen
                { }
            );

            smos-scheduler =
              final.callCabal2nix
                "smos-scheduler" ./smos-scheduler
                { };

            smos-single =
              final.callCabal2nix
                "smos-single" ./smos-single
                { };

            smos-sync-client =
              final.callCabal2nix
                "smos-sync-client" ./smos-sync-client
                { };

            smos-sync-client-gen =
              final.callCabal2nix
                "smos-sync-client-gen" ./smos-sync-client-gen
                { };

            smos-web-style =
              final.callCabal2nix
                "smos-web-style" ./smos-web-style
                { };
          });
      in
      {
        packages = {
          smos-archive = haskellPackages.smos-archive;
          smos-calendar-import = haskellPackages.smos-calendar-import;
          smos-github = haskellPackages.smos-github;
          smos-notify = haskellPackages.smos-notify;
          smos-query = haskellPackages.smos-query;
          smos-scheduler = haskellPackages.smos-scheduler;
          smos-single = haskellPackages.smos-single;
          smos-sync-client = haskellPackages.smos-sync-client;
          smos = haskellPackages.smos;
        };

        defaultPackage = nixpkgs.legacyPackages.${system}.symlinkJoin {
          name = self.packages.${system}.smos.name;
          paths = with self.packages.${system}; [
            smos-archive
            smos-calendar-import
            smos-github
            smos-notify
            smos-query
            smos-scheduler
            smos-single
            smos-sync-client
            smos
          ];
        };
      }
    );
}
