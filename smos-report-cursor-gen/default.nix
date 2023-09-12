{ mkDerivation, base, containers, criterion, cursor, cursor-gen
, genvalidity, genvalidity-containers, genvalidity-criterion
, genvalidity-path, genvalidity-sydtest, genvalidity-sydtest-lens
, lib, microlens, path, QuickCheck, smos-data, smos-data-gen
, smos-directory, smos-directory-gen, smos-report
, smos-report-cursor, smos-report-gen, sydtest, sydtest-discover
, time
}:
mkDerivation {
  pname = "smos-report-cursor-gen";
  version = "0.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor cursor-gen genvalidity genvalidity-path
    microlens path QuickCheck smos-data smos-data-gen smos-report
    smos-report-cursor smos-report-gen
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-lens QuickCheck
    smos-data-gen smos-directory smos-directory-gen smos-report-cursor
    smos-report-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity genvalidity-containers
    genvalidity-criterion genvalidity-path smos-data smos-data-gen
    smos-report smos-report-cursor time
  ];
  license = lib.licenses.mit;
}
