{ mkDerivation, base, bytestring, lib, monad-logger, password, text
}:
mkDerivation {
  pname = "smos-cli";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring monad-logger password text
  ];
  license = lib.licenses.mit;
}
