{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, containers, cookie, deepseq, dirforest
, envparse, filelock, filepath, hostname, http-client
, http-client-tls, http-types, lib, mergeful, monad-logger, mtl
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, pretty-show, servant-auth-client
, servant-client, smos-api, smos-client, smos-data, smos-report
, text, time, unix, unliftio, validity, validity-bytestring
, validity-containers, validity-path, validity-uuid
}:
mkDerivation {
  pname = "smos-sync-client";
  version = "0.4.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers cookie
    deepseq dirforest envparse filelock filepath hostname http-client
    http-client-tls http-types mergeful monad-logger mtl
    optparse-applicative path path-io persistent persistent-sqlite
    pretty-show servant-auth-client servant-client smos-api smos-client
    smos-data smos-report text time unix unliftio validity
    validity-bytestring validity-containers validity-path validity-uuid
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-sync-client";
}
