(import ./nix/pkgs.nix).mkShell {
  inherit ((import ./.).pre-commit-check) shellHook;
}
