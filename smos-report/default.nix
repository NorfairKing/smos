{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, conduit
, containers, cursor, deepseq, dlist, envparse, IntervalMap, lib
, megaparsec, microlens, optparse-applicative, parsec, path
, pretty-show, safe, smos-data, smos-directory, text, time, tz
, validity, validity-path, validity-time, yaml
}:
mkDerivation {
  pname = "smos-report";
  version = "0.11.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base conduit containers cursor
    deepseq dlist envparse IntervalMap megaparsec microlens
    optparse-applicative parsec path pretty-show safe smos-data
    smos-directory text time tz validity validity-path validity-time
    yaml
  ];
  license = lib.licenses.mit;
}
