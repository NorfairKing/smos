let
  pkgs = import ./nix/pkgs.nix;
in
pkgs.mkShell {
  name = "smos-web-server-front-shell";
  buildInputs =
    with pkgs;
    [
      nodejs-12_x
      yarn
      yarn2nix
      purs
      purty
      spago
      spago2nix
    ];
}
