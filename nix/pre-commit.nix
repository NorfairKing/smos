let
  pre-commit-hooks = import (
    builtins.fetchTarball {
      url = "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/f709c4652d4696dbe7c6a8354ebd5938f2bf807b.tar.gz";
      sha256 = "sha256:0700c5awc2gjzgikhx69vjbpyshx6b5xljmpxrdzpgqyg3blxbkl";
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
