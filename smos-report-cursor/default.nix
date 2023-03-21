{ mkDerivation, base, conduit, containers, cursor, deepseq, lib
, microlens, path, smos-cursor, smos-data, smos-directory
, smos-report, time, tz, validity, validity-path
}:
mkDerivation {
  pname = "smos-report-cursor";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base conduit containers cursor deepseq microlens path smos-cursor
    smos-data smos-directory smos-report time tz validity validity-path
  ];
  license = lib.licenses.mit;
}
