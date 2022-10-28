{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, conduit
, containers, cursor, deepseq, envparse, hashable, lib, megaparsec
, microlens, optparse-applicative, parsec, path, path-io
, pretty-show, safe, smos-data, text, time, validity, validity-path
, validity-time, yaml
}:
mkDerivation {
  pname = "smos-report";
  version = "0.9.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base conduit containers cursor
    deepseq envparse hashable megaparsec microlens optparse-applicative
    parsec path path-io pretty-show safe smos-data text time validity
    validity-path validity-time yaml
  ];
  testHaskellDepends = [ base ];
  license = lib.licenses.mit;
}
