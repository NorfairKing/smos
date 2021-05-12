let
  sources = import ../nix/sources.nix;
  niv-overlay = final: previous: {
    niv = (import sources.niv { }).niv;
  };
  pkgs = import sources.nixpkgs { overlays = [ niv-overlay ]; };
in
pkgs.mkShell {
  name = "niv-update-shell";
  buildInputs = with pkgs; [
    gitAndTools.gh
    niv
  ];
}
