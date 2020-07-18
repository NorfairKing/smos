let
  pkgs = import ./nix/pkgs.nix;
  nix-tools-repo = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "nix-tools";
    rev = "595ca11bfe79fd929c1a18e07a4fdc0e3363eb82";
    sha256 = "sha256:1s7kwpc4x7gx4ix29n3kdf81516jpv2bs7kkv557q1zjlz7ll8kf";
  };
  nix-tools = import nix-tools-repo {};
  shellHook = (import ./.).pre-commit-check.shellHook;
in
nix-tools.nix-tools.components.exes.stack-to-nix
#  pkgs.mkShell {
#    name = "smos-nix-shell";
#    buildInputs = [
#      nix-tools
#    ];
#    shellHook = ''
#      ${shellHook}
#    '';
#  }
