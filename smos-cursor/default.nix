{ mkDerivation, base, containers, cursor, cursor-dirforest
, cursor-fuzzy-time, deepseq, dirforest, exceptions, filelock
, fuzzy-time, lib, microlens, path, path-io, resourcet
, smos-archive, smos-data, text, time, tz, validity, validity-time
}:
mkDerivation {
  pname = "smos-cursor";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers cursor cursor-dirforest cursor-fuzzy-time deepseq
    dirforest exceptions filelock fuzzy-time microlens path path-io
    resourcet smos-archive smos-data text time tz validity
    validity-time
  ];
  homepage = "https://github.com/NorfairKing/smos#readme";
  license = "unknown";
}
