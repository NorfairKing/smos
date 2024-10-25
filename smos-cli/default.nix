{ mkDerivation, autodocodec, base, bytestring, genvalidity-sydtest
, genvalidity-text, http-client, http-types, lib, monad-logger
, opt-env-conf, path, path-io, pretty-show, retry
, safe-coloured-text, safe-coloured-text-layout, servant-client
, smos-data, sydtest, sydtest-discover, text, time, unliftio
, validity
}:
mkDerivation {
  pname = "smos-cli";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    autodocodec base bytestring http-client http-types monad-logger
    opt-env-conf path path-io pretty-show retry safe-coloured-text
    safe-coloured-text-layout servant-client smos-data text time
    unliftio validity
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-text sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
