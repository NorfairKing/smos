{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, base, bytestring, containers, deepseq, lib, microlens, path
, path-io, semver, text, time, tz, unliftio, validity
, validity-containers, validity-text, validity-time, yaml
}:
mkDerivation {
  pname = "smos-data";
  version = "0.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    containers deepseq microlens path path-io semver text time tz
    unliftio validity validity-containers validity-text validity-time
    yaml
  ];
  license = lib.licenses.mit;
}
