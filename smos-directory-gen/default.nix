{ mkDerivation, base, bytestring, containers, dirforest
, genvalidity, genvalidity-bytestring, genvalidity-containers
, genvalidity-dirforest, genvalidity-path, genvalidity-sydtest, lib
, path, path-io, QuickCheck, smos-data, smos-data-gen
, smos-directory, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "smos-directory-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers dirforest genvalidity
    genvalidity-bytestring genvalidity-containers genvalidity-dirforest
    genvalidity-path genvalidity-sydtest path path-io QuickCheck
    smos-data smos-data-gen smos-directory
  ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
