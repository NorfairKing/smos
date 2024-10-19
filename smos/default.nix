{ mkDerivation, aeson, async, autodocodec, base, brick, bytestring
, conduit, containers, cursor, cursor-brick, cursor-dirforest
, cursor-dirforest-brick, cursor-fuzzy-time, directory, envparse
, exceptions, filepath, fuzzy-time, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, genvalidity-unordered-containers, lib, megaparsec, microlens
, monad-logger, mtl, opt-env-conf, optparse-applicative, path
, path-io, pretty-relative-time, process, QuickCheck, resourcet
, smos-archive, smos-cli, smos-cursor, smos-cursor-gen, smos-data
, smos-data-gen, smos-directory, smos-report, smos-report-cursor
, smos-report-gen, sydtest, sydtest-discover, text, time, tz
, validity, vty, vty-crossplatform
}:
mkDerivation {
  pname = "smos";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec base brick bytestring conduit containers
    cursor cursor-brick cursor-dirforest cursor-dirforest-brick
    cursor-fuzzy-time directory envparse exceptions filepath fuzzy-time
    megaparsec microlens monad-logger mtl opt-env-conf
    optparse-applicative path path-io pretty-relative-time process
    resourcet smos-archive smos-cli smos-cursor smos-data
    smos-directory smos-report smos-report-cursor text time tz validity
    vty vty-crossplatform
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring containers genvalidity genvalidity-containers
    genvalidity-path genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text genvalidity-time genvalidity-unordered-containers
    megaparsec path path-io QuickCheck resourcet smos-cursor-gen
    smos-data smos-data-gen smos-report-gen sydtest text vty
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos";
}
