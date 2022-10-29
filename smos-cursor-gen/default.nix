{ mkDerivation, base, bytestring, criterion, cursor
, cursor-dirforest, cursor-dirforest-gen, cursor-fuzzy-time-gen
, cursor-gen, dirforest, genvalidity, genvalidity-criterion
, genvalidity-path, genvalidity-sydtest, genvalidity-sydtest-lens
, genvalidity-text, lib, path, path-io, QuickCheck, resourcet
, smos-cursor, smos-data, smos-data-gen, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "smos-cursor-gen";
  version = "0.2.1";
  src = ./.;
  libraryHaskellDepends = [
    base cursor cursor-dirforest-gen cursor-fuzzy-time-gen cursor-gen
    genvalidity genvalidity-text QuickCheck smos-cursor smos-data
    smos-data-gen
  ];
  testHaskellDepends = [
    base bytestring cursor cursor-dirforest cursor-fuzzy-time-gen
    cursor-gen dirforest genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-lens path path-io QuickCheck resourcet
    smos-cursor smos-data smos-data-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion smos-cursor smos-data
    smos-data-gen
  ];
  homepage = "https://github.com/NorfairKing/smos#readme";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
