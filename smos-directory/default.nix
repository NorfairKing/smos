{ mkDerivation, autodocodec, base, conduit, containers, cursor
, envparse, lib, microlens, optparse-applicative, path, path-io
, smos-data, validity, validity-path
}:
mkDerivation {
  pname = "smos-directory";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base conduit containers cursor envparse microlens
    optparse-applicative path path-io smos-data validity validity-path
  ];
  license = lib.licenses.mit;
}
