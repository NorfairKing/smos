let pkgs = import (../default.nix);
in rec {
  smos-static = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.smos;
  release-target = pkgs.stdenv.mkDerivation {
      name = "smos-release";
      buildInputs = [ smos-static ];
      nativeBuildInputs =
           pkgs.lib.attrsets.attrValues pkgs.smosPackages
        ++ pkgs.lib.attrsets.attrValues pkgs.cursorPackages
        ++ pkgs.lib.attrsets.attrValues pkgs.fuzzyTimePackages
        ++ pkgs.lib.attrsets.attrValues pkgs.cursorFuzzyTimePackages;
      buildCommand = ''
        cp -r ${smos-static} $out
      '';
    };
}
