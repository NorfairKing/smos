let
  pkgs = import ./nix/pkgs.nix { static = false; };
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pkgs.mkShell {
  name = "smos-nix-shell";
  buildInputs = pre-commit-hooks.tools;
  shellHook = ''
    ${pre-commit-hooks.run.shellHook}


    function nix-build_ {
      nix-build \
        --option extra-substituters https://iohk.cachix.org \
        --option trusted-public-keys iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= \
        --option extra-substituters https://smos.cachix.org \
        --option trusted-public-keys smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM= \
        $*
    }
    alias nix-build=nix-build_
  '';
}
