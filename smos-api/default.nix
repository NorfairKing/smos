{ mkDerivation, aeson, autodocodec, base, base64-bytestring
, bytestring, containers, cryptonite, deepseq, dirforest
, http-api-data, ical, lib, memory, mergeful, mergeful-persistent
, password, password-instances, path, path-pieces, persistent
, semver, servant, servant-auth, servant-auth-server, smos-data
, smos-report, text, time, typed-uuid, tz, uuid, validity
, validity-bytestring, validity-path, validity-text, validity-uuid
, yaml
}:
mkDerivation {
  pname = "smos-api";
  version = "0.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base64-bytestring bytestring containers
    cryptonite deepseq dirforest http-api-data ical memory mergeful
    mergeful-persistent password password-instances path path-pieces
    persistent semver servant servant-auth servant-auth-server
    smos-data smos-report text time typed-uuid tz uuid validity
    validity-bytestring validity-path validity-text validity-uuid yaml
  ];
  license = lib.licenses.mit;
}
