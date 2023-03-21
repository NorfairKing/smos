{ mkDerivation, base, bytestring, genvalidity-sydtest, lib
, monad-logger, password, sydtest, sydtest-discover, text, validity
}:
mkDerivation {
  pname = "smos-cli";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring monad-logger password text validity
  ];
  testHaskellDepends = [ base genvalidity-sydtest sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
}
