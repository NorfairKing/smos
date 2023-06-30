{ mkDerivation, aeson, autodocodec, autodocodec-yaml, autoexporter
, base, base16-bytestring, bytestring, conduit, containers
, data-default, deepseq, envparse, genvalidity-sydtest, http-client
, http-client-tls, http-types, ical, lib, monad-logger, mtl
, optparse-applicative, path, path-io, persistent-sqlite
, pretty-relative-time, pretty-show, QuickCheck, servant
, servant-auth-client, servant-client, shakespeare, smos, smos-cli
, smos-client, smos-data, smos-data-gen, smos-directory
, smos-report, smos-server-gen, smos-sync-client, smos-web-style
, stm, sydtest, sydtest-discover, sydtest-servant, sydtest-yesod
, template-haskell, text, time, typed-uuid, tz, unliftio, wai-extra
, warp, yaml, yesod, yesod-auth, yesod-autoreload, yesod-static
, yesod-static-remote, yesod-websockets
}:
mkDerivation {
  pname = "smos-web-server";
  version = "0.10.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base base16-bytestring
    bytestring conduit containers data-default deepseq envparse
    http-client http-client-tls http-types ical monad-logger mtl
    optparse-applicative path path-io persistent-sqlite
    pretty-relative-time pretty-show servant servant-auth-client
    servant-client shakespeare smos smos-cli smos-client smos-data
    smos-directory smos-report smos-sync-client smos-web-style stm
    template-haskell text time typed-uuid tz unliftio wai-extra warp
    yaml yesod yesod-auth yesod-autoreload yesod-static
    yesod-static-remote yesod-websockets
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client http-types
    monad-logger mtl path path-io QuickCheck servant-client smos-client
    smos-data smos-data-gen smos-server-gen smos-web-style stm sydtest
    sydtest-servant sydtest-yesod text yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-web-server";
}
