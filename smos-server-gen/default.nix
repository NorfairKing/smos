{ mkDerivation, base, bytestring, containers, deepseq, dirforest
, genvalidity, genvalidity-bytestring, genvalidity-persistent
, genvalidity-sydtest, genvalidity-time, genvalidity-typed-uuid
, http-client, http-client-tls, http-types, lib, mergeful, mtl
, path, path-io, persistent, persistent-sqlite, QuickCheck, semver
, servant, servant-auth-client, servant-auth-server, servant-client
, smos-api, smos-api-gen, smos-client, smos-data, smos-data-gen
, smos-report, smos-report-gen, smos-server, smos-sync-client
, sydtest, sydtest-discover, sydtest-persistent-sqlite, sydtest-wai
, text, time, typed-uuid, unliftio, zip, zstd
}:
mkDerivation {
  pname = "smos-server-gen";
  version = "0.6.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq dirforest genvalidity
    genvalidity-persistent genvalidity-time genvalidity-typed-uuid
    http-client mergeful path persistent-sqlite servant-auth-client
    servant-auth-server smos-api-gen smos-client smos-data
    smos-data-gen smos-report-gen smos-server smos-sync-client sydtest
    sydtest-persistent-sqlite sydtest-wai unliftio
  ];
  executableHaskellDepends = [
    base http-client-tls http-types mtl semver servant-client
    smos-client smos-data sydtest text
  ];
  executableToolDepends = [ sydtest-discover ];
  testHaskellDepends = [
    base bytestring containers dirforest genvalidity-bytestring
    genvalidity-persistent genvalidity-sydtest http-types mergeful mtl
    path path-io persistent QuickCheck servant servant-client smos-api
    smos-client smos-data smos-data-gen smos-report smos-report-gen
    smos-server sydtest sydtest-persistent-sqlite time typed-uuid zip
    zstd
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
