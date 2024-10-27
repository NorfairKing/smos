{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, conduit
, containers, cursor, deepseq, dlist, IntervalMap, lib, megaparsec
, microlens, opt-env-conf, parsec, path, pretty-show, safe
, smos-data, smos-directory, text, time, tz, validity
, validity-path, validity-time, yaml
}:
mkDerivation {
  pname = "smos-report";
  version = "0.12.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base conduit containers cursor
    deepseq dlist IntervalMap megaparsec microlens opt-env-conf parsec
    path pretty-show safe smos-data smos-directory text time tz
    validity validity-path validity-time yaml
  ];
  license = lib.licenses.mit;
}
