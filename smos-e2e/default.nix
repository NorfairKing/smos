{ mkDerivation, aeson, async, base, bytestring, conduit, dirforest
, filelock, genvalidity, genvalidity-containers, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, genvalidity-unordered-containers, lib, mtl
, path, path-io, QuickCheck, smos, smos-data, smos-data-gen
, smos-directory, sydtest, sydtest-discover, text, time, unix
, unliftio, vty, yaml
}:
mkDerivation {
  pname = "smos-e2e";
  version = "0.8.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring conduit smos unix unliftio vty
  ];
  testHaskellDepends = [
    aeson async base bytestring conduit dirforest filelock genvalidity
    genvalidity-containers genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time
    genvalidity-unordered-containers mtl path path-io QuickCheck smos
    smos-data smos-data-gen smos-directory sydtest text time unliftio
    yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
