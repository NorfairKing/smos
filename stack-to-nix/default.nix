{
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (
    builtins.fetchTarball
      {
        url = "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
        sha256 = "sha256:13jx33vj8ixr3m88181wybzb8qlx0rl249bpphgz944anxp8z521";
      }
  ) {}

  # haskell.nix provides access to the nixpkgs pins which are used by our CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

  # import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "smos";
    src = ../.;
  };
}
