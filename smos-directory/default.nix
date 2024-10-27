{ mkDerivation, autodocodec, base, conduit, containers, cursor, lib
, microlens, opt-env-conf, path, path-io, smos-data, validity
}:
mkDerivation {
  pname = "smos-directory";
  version = "0.2.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base conduit containers cursor microlens opt-env-conf
    path path-io smos-data validity
  ];
  license = lib.licenses.mit;
}
