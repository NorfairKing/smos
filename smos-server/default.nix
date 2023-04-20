{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, conduit, containers, dirforest
, envparse, filepath, http-client, IntervalMap, jose, lib, looper
, mergeful, microlens, monad-logger, mtl, optparse-applicative
, path, path-io, persistent, persistent-sqlite, pretty-show, semver
, servant, servant-auth-server, servant-server, smos-api, smos-cli
, smos-data, smos-directory, smos-report, smos-stripe-client
, template-haskell, text, time, typed-uuid, unliftio, validity
, validity-persistent, wai, wai-extra, warp, zip, zstd
}:
mkDerivation {
  pname = "smos-server";
  version = "0.11.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    conduit containers dirforest envparse filepath http-client
    IntervalMap jose looper mergeful microlens monad-logger mtl
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-show semver servant servant-auth-server servant-server
    smos-api smos-cli smos-data smos-directory smos-report
    smos-stripe-client template-haskell text time typed-uuid unliftio
    validity validity-persistent wai wai-extra warp zip zstd
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-server";
}
