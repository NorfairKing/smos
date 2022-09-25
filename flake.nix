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
      hsPkgs = pkgs.haskell.packages.ghc942.override {
        overrides = hfinal: hprev:
          horizon-platform.packages.${system} //
          {
            smos-report = hprev.callCabal2nix "smos-report" ./smos-report { };
          };
      };
    in
    {
      devShells.default = hsPkgs.smos-report.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          hsPkgs.cabal-install
          hsPkgs.stylish-haskell
          pkgs.nixpkgs-fmt
        ];
      });
      packages.default = hsPkgs.smos-report;
      checks =
        {
          hlint = lint-utils.outputs.linters.${system}.hlint self;
          hpack = lint-utils.outputs.linters.${system}.hpack self;
          nixpkgs-fmt = lint-utils.outputs.linters.${system}.nixpkgs-fmt self;
          stylish-haskell = lint-utils.outputs.linters.${system}.stylish-haskell self;
        };
    });
}
