let
  sources = import ./sources.nix;
  pre-commit-hooks = import sources.nix-pre-commit-hooks;
in
{
  run = pre-commit-hooks.run {
    src = ../.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
      hpack.enable = true;
    };
  };
  tools = [
    pre-commit-hooks.hlint
    pre-commit-hooks.nixpkgs-fmt
    pre-commit-hooks.ormolu
  ];
}
