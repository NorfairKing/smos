let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pkgs.mkShell {
  name = "smos-nix-shell";
  buildInputs = pre-commit-hooks.tools;
  shellHook = ''
    ${pre-commit-hooks.run.shellHook}
  '';
}
