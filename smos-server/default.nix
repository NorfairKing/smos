{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-ses
, autodocodec, autodocodec-yaml, autoexporter, base, bytestring
, conduit, containers, dirforest, envparse, filepath, http-client
, http-types, ical, IntervalMap, jose, lib, looper, mergeful
, microlens, monad-logger, mtl, network-uri, optparse-applicative
, path, path-io, persistent, persistent-sqlite, pretty-show, retry
, semver, servant, servant-auth-server, servant-server, smos-api
, smos-cli, smos-data, smos-directory, smos-report
, smos-stripe-client, template-haskell, text, time, typed-uuid, tz
, unliftio, validity, validity-persistent, wai, wai-extra, warp
, zip, zstd
}:
mkDerivation {
  pname = "smos-server";
  version = "0.11.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-ses autodocodec
    autodocodec-yaml base bytestring conduit containers dirforest
    envparse filepath http-client http-types ical IntervalMap jose
    looper mergeful microlens monad-logger mtl network-uri
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-show retry semver servant servant-auth-server servant-server
    smos-api smos-cli smos-data smos-directory smos-report
    smos-stripe-client template-haskell text time typed-uuid tz
    unliftio validity validity-persistent wai wai-extra warp zip zstd
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-server";
}
