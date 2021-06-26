let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; static = false; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "smos-nix-shell";
  buildInputs = with pkgs; [
    asciinema
    autoconf
    bzip2
    git
    haskellPackages.autorecorder
    haskellPackages.autoexporter
    killall
    niv
    sass
    stripe-cli
    x11
    xorg.libXScrnSaver
    xorg.libXrandr
    xorg.libXrender
    zlib
  ] ++ pre-commit.tools;
  shellHook = ''
    ${pre-commit.run.shellHook}


    function nix-build_ {
      nix-build \
        --option extra-substituters https://iohk.cachix.org \
        --option trusted-public-keys iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= \
        --option extra-substituters https://validity.cachix.org \
        --option trusted-public-keys validity.cachix.org-1:CqZp6vt9ir3yB5f8GAtfkJxPZG8hKC5fhIdaQsf7eZE= \
        --option extra-substituters https://dirforest.cachix.org \
        --option trusted-public-keys dirforest.cachix.org-1:C/TnLXGIkOL7jrhIZo95ahDttiZIc6XPaMV9xWQJddY= \
        --option extra-substituters https://cursor.cachix.org \
        --option trusted-public-keys cursor.cachix.org-1:1mqR0v1xbBZm08uXByCpaCm/zom3/HZkP4NXevS+kv8= \
        --option extra-substituters https://cursor-dirforest.cachix.org \
        --option trusted-public-keys cursor-dirforest.cachix.org-1:f/YRo6J0qWCFwGEzoONSWXs0hIYYZdPFuZaN6zm0nzQ= \
        --option extra-substituters https://mergeful.cachix.org \
        --option trusted-public-keys mergeful.cachix.org-1:M7dKd3h2zI+7jGWyqFCcUutbXRtgPgMnDS4XAZQlCXU= \
        --option extra-substituters https://yamlparse.cachix.org \
        --option trusted-public-keys yamlparse.cachix.org-1:DLkIYUWCK4HdTen7mwYsf2LB8o+REcV73MONfnAtQsY= \
        --option extra-substituters https://smos.cachix.org \
        --option trusted-public-keys smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM= \
        $*
    }
    alias nix-build=nix-build_
  '';
}
