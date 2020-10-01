{ static ? false
}:
let
  # A very specific version of nixpkgs.

  # This comes from https://github.com/srid/neuron/blob/static/nix/sources.json#L32-L33
  # See https://github.com/srid/neuron/pull/417/files
  nixpkgs-special = import (
    builtins.fetchTarball {
      url = "https://github.com/srid/nixpkgs/archive/312f5dc940b1a2c627e8cce4adc192cfa3e730db.tar.gz";
      sha256 = "1x1h6j43wp2fgzjlv0nf8h5syvpdp3nhp8xb85hxzdz8k7hkhi4s";
    }
  );
  pkgsv = nixpkgs-special;


  # Haskell.nix
  # This is the latest version at the time of writing, nothing special.
  owner = "input-output-hk";
  repo = "haskell.nix";
  rev = "48b8674f5f726cfb5083c025d3c53ff01fef009a";
  sha256 = "sha256:0b90xnxn72kv5qskp3gxfcmql8cqbank7nlp0m6353yhqp6kr5mc";
  haskellNixV = import (
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    }
  );

  # When using 'pkgsMusl' instead of 'pkgsCross.musl64', I'm getting this error while building ghc:
  #
  # /nix/store/382fnvik7v4zfm659rzm51xf45ljq5ij-binutils-2.31.1/bin/ld: /nix/store/5w14whxfj3dq1ah3fmbyp9m9azxyd4zp-ghc-8.4.4/lib/ghc-8.4.4/rts/libHSrts.a(Hpc.o): in function `startupHpc':
  # /home/ben/bin-dist-8.4.4-Linux/ghc/rts/Hpc.c:214:0: error:
  #      undefined reference to `__strdup'
  # 
  # nomeata advises to use pkgs.Cross.musl64 instead, here: 
  # https://github.com/NixOS/nixpkgs/issues/57238#issuecomment-473513922
  #
  # However, this was on a closed issue and static-haskell-nix uses pkgsMusl.

  haskellNix = haskellNixV {
    pkgs =
      if static
      then nixpkgs-special.pkgsCross.musl64
      else haskellNixV.sources.nixpkgs;
  };

in
haskellNix.pkgs
