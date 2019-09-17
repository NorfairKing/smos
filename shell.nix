(import <nixpkgs> {}).mkShell {
  inherit ((import ./.).pre-commit-check) shellHook;
}
