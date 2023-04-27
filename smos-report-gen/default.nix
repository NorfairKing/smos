{ mkDerivation, base, containers, criterion, cursor, cursor-gen
, deepseq, dirforest, dlist, genvalidity, genvalidity-containers
, genvalidity-criterion, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-time, IntervalMap, lib
, megaparsec, parsec, path, QuickCheck, smos-data, smos-data-gen
, smos-directory, smos-directory-gen, smos-report, sydtest
, sydtest-discover, text, time, tz
}:
mkDerivation {
  pname = "smos-report-gen";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor cursor-gen deepseq dlist genvalidity
    genvalidity-containers genvalidity-path genvalidity-time
    IntervalMap path QuickCheck smos-data smos-data-gen
    smos-directory-gen smos-report text time
  ];
  testHaskellDepends = [
    base containers cursor cursor-gen dirforest genvalidity-path
    genvalidity-sydtest genvalidity-sydtest-aeson megaparsec parsec
    path QuickCheck smos-data smos-data-gen smos-directory
    smos-directory-gen smos-report sydtest text time tz
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base containers criterion cursor genvalidity-containers
    genvalidity-criterion genvalidity-path path smos-data smos-data-gen
    smos-report text
  ];
  license = lib.licenses.mit;
}
