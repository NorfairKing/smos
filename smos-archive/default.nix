{ mkDerivation, autodocodec, base, bytestring, conduit, containers
, dirforest, envparse, exceptions, filepath, genvalidity-sydtest
, lib, monad-logger, mtl, optparse-applicative, path, path-io
, smos-cli, smos-data, smos-data-gen, smos-directory
, smos-directory-gen, smos-report, sydtest, sydtest-discover, text
, time, unliftio
}:
mkDerivation {
  pname = "smos-archive";
  version = "0.3.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base bytestring conduit containers envparse exceptions
    filepath monad-logger mtl optparse-applicative path path-io
    smos-cli smos-data smos-directory smos-report text time unliftio
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring dirforest genvalidity-sydtest monad-logger path
    path-io smos-data smos-data-gen smos-directory smos-directory-gen
    smos-report sydtest time
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-archive";
}
