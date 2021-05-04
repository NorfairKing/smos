let
  sources = import ./sources.nix;
  pre-commit-hooks = import sources.nix-pre-commit-hooks;
in
{
  run = pre-commit-hooks.run {
    src = ../.;
    hooks = {
      hlint.enable = true;
      hpack.enable = true;
      nixpkgs-fmt.enable = true;
      ormolu.enable = true;
    };
  };
  tools = [
    pre-commit-hooks.hlint
    pre-commit-hooks.hpack
    pre-commit-hooks.nixpkgs-fmt
    pre-commit-hooks.ormolu
  ];
}
