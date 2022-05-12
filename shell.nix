{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "smos-nix-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    asciinema
    autoconf
    bzip2
    feedback
    git
    haskellPackages.autoexporter
    haskellPackages.autorecorder
    hub
    killall
    sass
    stripe-cli
    x11
    xorg.libXScrnSaver
    xorg.libXrandr
    xorg.libXrender
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + pkgs.feedback.shellHook;
}
