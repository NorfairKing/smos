{ mkDerivation, aeson, autodocodec, base, conduit, containers
, cursor, envparse, lib, microlens, optparse-applicative, path
, path-io, smos-data, text, validity, validity-path, validity-text
}:
mkDerivation {
  pname = "smos-directory";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base conduit containers cursor envparse microlens
    optparse-applicative path path-io smos-data text validity
    validity-path validity-text
  ];
  license = lib.licenses.mit;
}
