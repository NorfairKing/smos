let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "smos-nix-shell";
  buildInputs = with pkgs; [
    asciinema
    autoconf
    bzip2
    git
    haskellPackages.autoexporter
    haskellPackages.autoexporter
    haskellPackages.feedback
    killall
    (import sources.niv { }).niv
    sass
    stripe-cli
    x11
    xorg.libXScrnSaver
    xorg.libXrandr
    xorg.libXrender
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
