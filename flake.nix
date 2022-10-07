{
  description = "smos";
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    horizon-platform = {
      url = "git+https://gitlab.homotopic.tech/horizon/horizon-platform?ref=smos";
    };
    lint-utils = {
      url = "git+https://gitlab.homotopic.tech/nix/lint-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs =
    inputs@
    { self
    , flake-utils
    , horizon-platform
    , lint-utils
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hsPkgs =
        with pkgs.haskell.lib.compose;
        pkgs.haskell.packages.ghc942.override {
          overrides = hfinal: hprev:
            horizon-platform.packages.${system} //
            {
              smos-archive = addBuildTool hprev.autoexporter (hprev.callCabal2nix "smos-archive" ./smos-archive { });
              smos-cursor = addBuildTool hprev.autoexporter (hprev.callCabal2nix "smos-cursor" ./smos-cursor { });
              smos-cursor-gen = addBuildTool hprev.autoexporter (hprev.callCabal2nix "smos-cursor-gen" ./smos-cursor-gen { });
              smos-report = hprev.callCabal2nix "smos-report" ./smos-report { };
              smos-report-cursor = hprev.callCabal2nix "smos-report-cursor" ./smos-report-cursor { };
              smos-report-gen = hprev.callCabal2nix "smos-report-gen" ./smos-report-gen { };
              smos = hprev.callCabal2nix "smos" ./smos { };
            };
      };
    in
    {
      devShells.default = hsPkgs.smos-archive.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          hsPkgs.cabal-install
          hsPkgs.stylish-haskell
          pkgs.nixpkgs-fmt
        ];
      });
      packages.default = hsPkgs.smos;
      checks =
        {
          hlint = lint-utils.outputs.linters.${system}.hlint self;
          hpack = lint-utils.outputs.linters.${system}.hpack self;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt self;
          stylish-haskell = lint-utils.outputs.linters.${system}.stylish-haskell self;
        };
    });
}
