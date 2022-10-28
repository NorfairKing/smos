{ mkDerivation, aeson, base, bytestring, containers, criterion
, dirforest, genvalidity, genvalidity-bytestring
, genvalidity-criterion, genvalidity-mergeful, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-typed-uuid, genvalidity-uuid, lib, mergeful, path
, QuickCheck, semver, smos-api, smos-data, smos-data-gen
, smos-report, sydtest, sydtest-aeson, sydtest-discover, text, time
, typed-uuid, uuid
}:
mkDerivation {
  pname = "smos-api-gen";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-mergeful
    genvalidity-path genvalidity-text genvalidity-typed-uuid
    genvalidity-uuid QuickCheck smos-api smos-data-gen text
  ];
  testHaskellDepends = [
    aeson base bytestring containers dirforest genvalidity-sydtest
    genvalidity-sydtest-aeson mergeful path semver smos-api smos-data
    smos-report sydtest sydtest-aeson time typed-uuid uuid
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion smos-api
  ];
  license = lib.licenses.mit;
}
