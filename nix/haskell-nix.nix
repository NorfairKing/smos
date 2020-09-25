let
  owner = "input-output-hk";
  repo = "haskell.nix";
  rev = "f4136211c933b444ab2e0f358abd223929970220";
  sha256 = "sha256:1b9nxzkg29hwczr6pb6a7arxka8z0swzq7b2bqyxqzr4qvpcjlc1";
  haskellNixV = import (
    builtins.fetchTarball {
      url =
        "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    }
  );
  pkgs = haskellNixV.sources.nixpkgs haskellNixV.nixpkgsArgs;
  haskellNix = haskellNixV { pkgs = pkgs.pkgsCross.musl64; };
in
haskellNix
