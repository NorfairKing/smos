{ mkDerivation, aeson, autodocodec, base, conduit, containers
, cursor, deepseq, envparse, hashable, lib, megaparsec, microlens
, optparse-applicative, parsec, path, pretty-show, safe, smos-data
, smos-directory, text, time, tz, validity, validity-path
, validity-time, yaml
}:
mkDerivation {
  pname = "smos-report";
  version = "0.10.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base conduit containers cursor deepseq envparse
    hashable megaparsec microlens optparse-applicative parsec path
    pretty-show safe smos-data smos-directory text time tz validity
    validity-path validity-time yaml
  ];
  license = lib.licenses.mit;
}
