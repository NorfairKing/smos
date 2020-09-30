let
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pre-commit-hooks.run
