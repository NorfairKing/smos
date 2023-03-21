{ mkDerivation, aeson, autodocodec, base, envparse, lib
, optparse-applicative, path, path-io, text, validity
, validity-path, validity-text
}:
mkDerivation {
  pname = "smos-directory";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base envparse optparse-applicative path path-io
    text validity validity-path validity-text
  ];
  license = lib.licenses.mit;
}
