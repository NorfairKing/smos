{ mkDerivation, aeson, autodocodec, autodocodec-yaml, autoexporter
, base, base16-bytestring, bytestring, conduit, containers
, data-default, deepseq, envparse, genvalidity-sydtest, http-client
, http-client-tls, http-types, lib, monad-logger, mtl
, optparse-applicative, path, path-io, persistent-sqlite
, pretty-relative-time, pretty-show, QuickCheck, servant
, servant-auth-client, servant-client, shakespeare, smos, smos-cli
, smos-client, smos-data-gen, smos-server-gen, smos-sync-client
, smos-web-style, stm, sydtest, sydtest-discover, sydtest-yesod
, template-haskell, text, time, typed-uuid, unliftio, wai-extra
, warp, yesod, yesod-auth, yesod-autoreload, yesod-static
, yesod-static-remote, yesod-websockets
}:
mkDerivation {
  pname = "smos-web-server";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base base16-bytestring
    bytestring conduit containers data-default deepseq envparse
    http-client http-client-tls http-types monad-logger mtl
    optparse-applicative path path-io persistent-sqlite
    pretty-relative-time pretty-show servant servant-auth-client
    servant-client shakespeare smos smos-cli smos-client
    smos-sync-client smos-web-style stm template-haskell text time
    typed-uuid unliftio wai-extra warp yesod yesod-auth
    yesod-autoreload yesod-static yesod-static-remote yesod-websockets
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest http-client http-types
    monad-logger QuickCheck servant-client smos-client smos-data-gen
    smos-server-gen smos-web-style stm sydtest sydtest-yesod text
    yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-web-server";
}
