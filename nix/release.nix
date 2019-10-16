let
  pkgs = import ( ./pkgs.nix );
in
  pkgs.stdenv.mkDerivation {
    name = "smos-release";
    buildInputs =
      pkgs.lib.mapAttrsToList (
        name: pk:
          pkgs.haskell.lib.justStaticExecutables pk
      ) pkgs.smosPackages;
    # Just to make sure that the test suites in these pass:
    nativeBuildInputs =
      let
        dependencyPkg = pkg: pkgs.haskell.lib.disableLibraryProfiling pkg;
      in
        map dependencyPkg (
          pkgs.lib.attrsets.attrValues pkgs.validityPackages
        ++ pkgs.lib.attrsets.attrValues pkgs.cursorPackages
        ++ pkgs.lib.attrsets.attrValues pkgs.cursorBrickPackages
        ++ pkgs.lib.attrsets.attrValues pkgs.fuzzyTimePackages
        ++ pkgs.lib.attrsets.attrValues pkgs.cursorFuzzyTimePackages
        ++ pkgs.lib.attrsets.attrValues pkgs.prettyRelativeTimePackages
        ++ pkgs.lib.attrsets.attrValues pkgs.mergefulPackages
        );
    buildCommand =
      ''
      mkdir -p $out/bin
      for i in $buildInputs
      do
        if [ -d "$i/bin" ]
        then
          cp $i/bin/* $out/bin/
        fi
      done
    '';
  }
