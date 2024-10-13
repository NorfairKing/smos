{ mkDerivation, base, bytestring, containers, cursor, dirforest
, genvalidity, genvalidity-bytestring, genvalidity-containers
, genvalidity-dirforest, genvalidity-path, genvalidity-sydtest, lib
, path, path-io, QuickCheck, smos-data, smos-data-gen
, smos-directory, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "smos-directory-gen";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers dirforest genvalidity
    genvalidity-bytestring genvalidity-dirforest genvalidity-path
    genvalidity-sydtest path path-io QuickCheck smos-data smos-data-gen
    smos-directory
  ];
  testHaskellDepends = [
    base containers cursor genvalidity-containers genvalidity-sydtest
    smos-data-gen smos-directory sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
