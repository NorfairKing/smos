{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, envparse, genvalidity-sydtest, genvalidity-text, lib
, monad-logger, optparse-applicative, password, path, path-io
, safe-coloured-text, safe-coloured-text-layout, sydtest
, sydtest-discover, text, validity
}:
mkDerivation {
  pname = "smos-cli";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring envparse
    monad-logger optparse-applicative password path path-io
    safe-coloured-text safe-coloured-text-layout text validity
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-text sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
