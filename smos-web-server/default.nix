{ mkDerivation, aeson, autodocodec-yaml, autoexporter, base
, bytestring, conduit, containers, data-default, deepseq
, genvalidity-sydtest, http-client, http-client-tls, http-types
, ical, lib, mergeful, monad-logger, mtl, opt-env-conf, path
, path-io, persistent-sqlite, pretty-relative-time, pretty-show
, QuickCheck, servant, servant-auth-client, servant-client
, shakespeare, smos, smos-api, smos-cli, smos-client, smos-data
, smos-data-gen, smos-directory, smos-e2e, smos-server-gen
, smos-sync-client, smos-web-assets, sydtest, sydtest-discover
, sydtest-yesod, template-haskell, text, time, typed-uuid, tz
, unliftio, wai-extra, warp, yaml, yesod, yesod-auth
, yesod-autoreload, yesod-static, yesod-static-remote
, yesod-websockets
}:
mkDerivation {
  pname = "smos-web-server";
  version = "0.13.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec-yaml base bytestring conduit containers
    data-default deepseq http-client http-client-tls http-types ical
    monad-logger mtl opt-env-conf path path-io persistent-sqlite
    pretty-relative-time pretty-show servant servant-auth-client
    servant-client shakespeare smos smos-cli smos-client smos-data
    smos-directory smos-e2e smos-sync-client smos-web-assets
    template-haskell text time typed-uuid tz unliftio wai-extra warp
    yaml yesod yesod-auth yesod-autoreload yesod-static
    yesod-static-remote yesod-websockets
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client http-types mergeful
    monad-logger mtl QuickCheck servant-client smos-api smos-client
    smos-data-gen smos-server-gen smos-web-assets sydtest sydtest-yesod
    text time tz yaml yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-web-server";
}
