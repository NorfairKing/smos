{ sources ? import ./sources.nix }:
let
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
  tools = with pre-commit-hooks; [
    pre-commit
    hlint
    hpack
    nixpkgs-fmt
    ormolu
  ];
}
