let
  pkgs = import ./nix/pkgs.nix;
  shellHook = (import ./.).pre-commit-check.shellHook;
in
pkgs.mkShell {
  name = "smos-nix-shell";
  buildInputs = [
    nix-tools
  ];
  shellHook = ''
    ${shellHook}
  '';
}
