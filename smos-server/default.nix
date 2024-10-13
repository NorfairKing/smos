{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-ses
, autodocodec, autoexporter, base, blaze-html, bytestring, conduit
, containers, dirforest, filepath, http-client, http-types, ical
, IntervalMap, jose, lib, looper, mergeful, microlens, mime-mail
, monad-logger, mtl, network-uri, opt-env-conf, path, path-io
, persistent, persistent-pagination, persistent-sqlite, pretty-show
, retry, semver, servant, servant-auth-server, servant-server
, shakespeare, smos-api, smos-cli, smos-data, smos-directory
, smos-report, smos-stripe-client, template-haskell, text, time
, typed-uuid, tz, unliftio, wai, wai-extra, warp, yaml, zip, zstd
}:
mkDerivation {
  pname = "smos-server";
  version = "0.12.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-ses autodocodec base
    blaze-html bytestring conduit containers dirforest filepath
    http-client http-types ical IntervalMap jose looper mergeful
    microlens mime-mail monad-logger mtl network-uri opt-env-conf path
    path-io persistent persistent-pagination persistent-sqlite
    pretty-show retry semver servant servant-auth-server servant-server
    shakespeare smos-api smos-cli smos-data smos-directory smos-report
    smos-stripe-client template-haskell text time typed-uuid tz
    unliftio wai wai-extra warp yaml zip zstd
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-server";
}
