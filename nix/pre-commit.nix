let
  pre-commit-hooks = import (
    builtins.fetchTarball {
      url = "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/1b11ce0f8c65dd3d8e9520e23c100b76d09a858b.tar.gz";
      sha256 = "sha256:0l2v8hsxrvj8w335xxxln49rpd9z5ncv6bl2wnk65zzzd4wa5rkm";
    }
  );
in
{
  run = pre-commit-hooks.run {
    src = ../.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
  tools = [
    pre-commit-hooks.hlint
    pre-commit-hooks.nixpkgs-fmt
    pre-commit-hooks.ormolu
  ];
}
