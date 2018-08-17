let pkgs = import (../overlay.nix);
in fix {
  smos-static = justStaticExecutables final.haskellPackages.smos;
  release-target = final.stdenv.mkDerivation {
      name = "smos-release";
      buildInputs = [ final.regtool-static ];
      nativeBuildInputs = [ 
        final.haskellPackages.smos
        final.haskellPackages.smos-data
        final.haskellPackages.smos-data-gen
        final.haskellPackages.smos-cursor
        final.haskellPackages.smos-cursor-gen
      ];
      buildCommand = ''
        cp -r ${smos-static} $out
      '';
    };
}
