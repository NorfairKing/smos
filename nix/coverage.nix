{ lib
, haskell
, stdenv
, ghc
, gnutar
, rsync
, writeText
, symlinkJoin
}:
{ name
, packages
}:
with lib;

# hpc-trans (or -fhpc): does a source-to-source transformation and Outputs
#
# At transformation-time:
# * .pix: program-index file
# * .mix: module-index file (Info about each tick-box inside a module)
# At runtime:
# * .tix: program-tix files (coverage record)
#
# Then we have:
# * hpc-report: To creat a report
# * hpc-markup: To markup the source

let
  modifyBuild = pkg: haskell.lib.overrideCabal pkg (old: {
    doCheck = true;
    doCoverage = true;
    doBenchmark = false;
    # TODO put sources in $out
    postInstall = (old.postFixup or "") + ''
      mkdir -p $out/dist/build/autogen/
      cp dist/build/autogen/Paths*.hs $out/dist/build/autogen
    '';
  });
  emptyTixFile = writeText "empty.tix" "Tix []";

  copySourceAndResultScript = pkg:
    let
      src = pkg.src;
      modifiedPkg = modifyBuild pkg;
    in
    ''
      echo "Result: ${modifiedPkg}"
      echo "Source: ${src}"

      echo "Outputting sources of ${src} to $(pwd)/srcdir"
      mkdir -p tempsrcdir
      tar xvf ${src} --directory tempsrcdir
      for i in tempsrcdir/*
      do
        rsync --recursive $i/ srcdir/
      done
      mkdir -p srcdir/dist/build/autogen
      cp ${modifiedPkg}/dist/build/autogen/Paths*.hs srcdir/dist/build/autogen
      rm -rf tempsrcdir

      echo "Putting hpc files of ${modifiedPkg} to $(pwd)/hpcdir"
      find ${modifiedPkg}/share/hpc/vanilla/mix -type f -path '*.mix' -exec cp {} $(pwd)/hpcdir \;

      echo "Putting tix files of ${modifiedPkg} in $(pwd)/tixdir"
      for i in ${modifiedPkg}/share/hpc/vanilla/tix/*/*.tix
      do
        cp $i tixdir
      done
    '';
in
stdenv.mkDerivation {
  inherit name;
  srcs = builtins.map (pkg: pkg.src) packages;
  buildInputs = [ ghc gnutar rsync ];
  buildCommand = ''
    set -ex

    mkdir srcdir
    mkdir hpcdir
    mkdir tixdir

    ${concatStringsSep "\n" (builtins.map copySourceAndResultScript packages)}

    # HPC combine can only combine two files at a time, so we have to do this
    # annoying shuffle :(
    cp ${emptyTixFile} combined.tix
    for i in tixdir/*.tix
    do
      echo "Combining $i"
      hpc combine --union --exclude=Main --output=temp.nix combined.tix $i
      mv temp.nix combined.tix
    done
    # cat combined.tix

    mkdir -p $out
    cp combined.tix $out

    exit 1
    hpc report --per-module --srcdir=./srcdir/src --hpcdir=./hpcdir combined.tix
  '';
}
