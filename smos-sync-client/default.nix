{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, containers, cookie, deepseq, dirforest
, filelock, filepath, hostname, http-client, http-client-tls
, http-types, lib, mergeful, monad-logger, mtl, opt-env-conf, path
, path-io, persistent, persistent-sqlite, pretty-show
, servant-auth-client, servant-client, smos-api, smos-cli
, smos-client, smos-directory, text, time, unix, unliftio, validity
, validity-bytestring, validity-containers, validity-path
, validity-uuid
}:
mkDerivation {
  pname = "smos-sync-client";
  version = "0.6.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring containers cookie
    deepseq dirforest filelock filepath hostname http-client
    http-client-tls http-types mergeful monad-logger mtl opt-env-conf
    path path-io persistent persistent-sqlite pretty-show
    servant-auth-client servant-client smos-api smos-cli smos-client
    smos-directory text time unix unliftio validity validity-bytestring
    validity-containers validity-path validity-uuid
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-sync-client";
}
