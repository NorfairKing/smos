{ mkDerivation, base, bytestring, cookie, deepseq, dirforest
, http-types, ical, lib, microlens, path, pretty-show, semver
, servant, servant-auth-client, servant-auth-server, servant-client
, servant-client-core, smos-api, smos-data, smos-report, sydtest
, sydtest-discover, text, time
}:
mkDerivation {
  pname = "smos-client";
  version = "0.5.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cookie deepseq dirforest http-types ical microlens
    path pretty-show semver servant servant-auth-client
    servant-auth-server servant-client servant-client-core smos-api
    smos-data smos-report text time
  ];
  testHaskellDepends = [ base smos-api smos-data sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
