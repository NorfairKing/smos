{ mkDerivation, base, conduit, containers, cursor, deepseq, dlist
, lib, microlens, path, smos-cursor, smos-data, smos-directory
, smos-report, time, tz, validity, validity-path
}:
mkDerivation {
  pname = "smos-report-cursor";
  version = "0.6.1";
  src = ./.;
  libraryHaskellDepends = [
    base conduit containers cursor deepseq dlist microlens path
    smos-cursor smos-data smos-directory smos-report time tz validity
    validity-path
  ];
  license = lib.licenses.mit;
}
