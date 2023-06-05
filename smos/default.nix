{ mkDerivation, aeson, async, autodocodec, base, brick, bytestring
, conduit, containers, cursor, cursor-brick, cursor-dirforest
, cursor-dirforest-brick, cursor-fuzzy-time, directory, dirforest
, envparse, exceptions, filelock, filepath, fuzzy-time, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, genvalidity-unordered-containers, lib, megaparsec, microlens
, monad-logger, mtl, optparse-applicative, path, path-io
, pretty-relative-time, process, QuickCheck, resourcet
, smos-archive, smos-cli, smos-cursor, smos-cursor-gen, smos-data
, smos-data-gen, smos-directory, smos-report, smos-report-cursor
, smos-report-gen, sydtest, sydtest-discover, text, time, tz, unix
, unliftio, validity, vty, yaml
}:
mkDerivation {
  pname = "smos";
  version = "0.7.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec base brick bytestring conduit containers
    cursor cursor-brick cursor-dirforest cursor-dirforest-brick
    cursor-fuzzy-time directory envparse exceptions filepath fuzzy-time
    megaparsec microlens monad-logger mtl optparse-applicative path
    path-io pretty-relative-time process resourcet smos-archive
    smos-cli smos-cursor smos-data smos-directory smos-report
    smos-report-cursor text time tz unix unliftio validity vty
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson async base bytestring conduit containers dirforest filelock
    genvalidity genvalidity-containers genvalidity-path
    genvalidity-sydtest genvalidity-sydtest-aeson genvalidity-text
    genvalidity-time genvalidity-unordered-containers megaparsec mtl
    path path-io QuickCheck resourcet smos-cursor-gen smos-data
    smos-data-gen smos-directory smos-report-gen sydtest text time
    unliftio vty yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos";
}
