{ mkDerivation, async, base, bytestring, containers, criterion
, dirforest, genvalidity, genvalidity-bytestring
, genvalidity-criterion, genvalidity-dirforest, genvalidity-path
, genvalidity-sydtest, jose, lib, mergeful, monad-logger, path
, path-io, persistent-sqlite, QuickCheck, resource-pool
, servant-auth-server, servant-client, smos-api, smos-api-gen
, smos-server, smos-server-gen, smos-sync-client, sydtest
, sydtest-discover, sydtest-persistent-sqlite, sydtest-wai, text
}:
mkDerivation {
  pname = "smos-sync-client-gen";
  version = "0.4.1";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring dirforest genvalidity genvalidity-dirforest
    genvalidity-sydtest monad-logger path path-io persistent-sqlite
    QuickCheck servant-client smos-api smos-api-gen smos-sync-client
    sydtest sydtest-persistent-sqlite
  ];
  testHaskellDepends = [
    base bytestring containers genvalidity-bytestring genvalidity-path
    genvalidity-sydtest jose mergeful monad-logger path path-io
    persistent-sqlite QuickCheck resource-pool servant-auth-server
    servant-client smos-api smos-api-gen smos-server smos-server-gen
    smos-sync-client sydtest sydtest-persistent-sqlite sydtest-wai text
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion smos-sync-client
  ];
  license = lib.licenses.mit;
}
