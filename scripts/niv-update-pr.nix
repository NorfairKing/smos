let
  sources = import ../nix/sources.nix;
  niv-overlay = final: previous: {
    niv = (import sources.niv { }).niv;
  };
  pkgs = import sources.nixpkgs { overlays = [ niv-overlay ]; };
  pre-commit = import ../nix/pre-commit.nix { inherit sources; };
in
pkgs.mkShell {
  name = "niv-update-shell";
  buildInputs = with pkgs; [
    gitAndTools.gh
    niv
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
