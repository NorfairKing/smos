{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, containers, criterion, filepath, genvalidity
, genvalidity-containers, genvalidity-criterion
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, lib, path, path-io, QuickCheck, semver
, smos-data, sydtest, sydtest-discover, text, time, tz
}:
mkDerivation {
  pname = "smos-data-gen";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers genvalidity genvalidity-containers genvalidity-text
    genvalidity-time path-io QuickCheck smos-data sydtest text time tz
  ];
  testHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring containers
    filepath genvalidity-sydtest genvalidity-sydtest-aeson path path-io
    QuickCheck semver smos-data sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base containers criterion genvalidity-containers
    genvalidity-criterion path path-io smos-data
  ];
  license = lib.licenses.mit;
}
