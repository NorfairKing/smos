{ sources ? import ./sources.nix }:
let
  pre-commit-hooks = import sources.pre-commit-hooks;
in
{
  run = pre-commit-hooks.run {
    src = ../.;
    hooks = {
      hlint.enable = true;
      hpack.enable = true;
      nixpkgs-fmt.enable = true;
      ormolu.enable = true;
      statix.enable = true;
    };
    settings.statix.ignore = [
      "sources.nix"
    ];
  };
  tools = with pre-commit-hooks; [
    hlint
    hpack
    nixpkgs-fmt
    ormolu
    pre-commit
    statix
  ];
}
