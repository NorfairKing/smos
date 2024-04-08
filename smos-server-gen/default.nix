{ mkDerivation, async, base, bytestring, containers, deepseq
, dirforest, genvalidity-bytestring, genvalidity-persistent
, genvalidity-sydtest, genvalidity-time, http-client
, http-client-tls, http-types, ical, lib, mergeful, mtl, path
, path-io, persistent, persistent-sqlite, QuickCheck, safe, semver
, servant, servant-auth-client, servant-auth-server, servant-client
, smos-api, smos-api-gen, smos-client, smos-data, smos-data-gen
, smos-directory-gen, smos-report, smos-server, smos-sync-client
, sydtest, sydtest-discover, sydtest-persistent-sqlite, sydtest-wai
, text, time, typed-uuid, tz, unliftio, uuid, yaml, zip, zstd
}:
mkDerivation {
  pname = "smos-server-gen";
  version = "0.7.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq dirforest genvalidity-sydtest
    http-client mergeful path persistent-sqlite QuickCheck
    servant-auth-client servant-auth-server smos-api-gen smos-client
    smos-data smos-data-gen smos-directory-gen smos-server
    smos-sync-client sydtest sydtest-persistent-sqlite sydtest-wai
    unliftio yaml
  ];
  executableHaskellDepends = [
    base http-client-tls http-types mtl semver servant-client
    smos-client smos-data sydtest text
  ];
  executableToolDepends = [ sydtest-discover ];
  testHaskellDepends = [
    async base bytestring containers dirforest genvalidity-bytestring
    genvalidity-persistent genvalidity-sydtest genvalidity-time
    http-types ical mergeful mtl path path-io persistent QuickCheck
    safe servant servant-client smos-api smos-client smos-data
    smos-data-gen smos-directory-gen smos-report smos-server sydtest
    sydtest-persistent-sqlite time typed-uuid tz uuid zip zstd
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-server-end-to-end-test";
}
