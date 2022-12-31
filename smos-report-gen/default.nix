{ mkDerivation, base, bytestring, containers, criterion, cursor
, cursor-gen, deepseq, dirforest, genvalidity
, genvalidity-bytestring, genvalidity-containers
, genvalidity-criterion, genvalidity-dirforest, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, lib, megaparsec, parsec, path, path-io
, QuickCheck, smos-data, smos-data-gen, smos-report, sydtest
, sydtest-discover, text, time, tz
}:
mkDerivation {
  pname = "smos-report-gen";
  version = "0.4.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers cursor cursor-gen deepseq dirforest
    genvalidity genvalidity-bytestring genvalidity-containers
    genvalidity-dirforest genvalidity-path genvalidity-sydtest
    genvalidity-text genvalidity-time path path-io QuickCheck smos-data
    smos-data-gen smos-report text time
  ];
  testHaskellDepends = [
    base containers cursor cursor-gen dirforest genvalidity-containers
    genvalidity-path genvalidity-sydtest genvalidity-sydtest-aeson
    megaparsec parsec path QuickCheck smos-data smos-data-gen
    smos-report sydtest text time tz
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base containers criterion cursor genvalidity-containers
    genvalidity-criterion genvalidity-path path smos-data smos-data-gen
    smos-report text
  ];
  license = lib.licenses.mit;
}
