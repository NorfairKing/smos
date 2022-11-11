{ mkDerivation, autodocodec, base, bytestring, conduit, containers
, dirforest, envparse, exceptions, filepath, genvalidity-sydtest
, lib, monad-logger, mtl, optparse-applicative, path, path-io
, smos-data, smos-data-gen, smos-report, smos-report-gen, sydtest
, sydtest-discover, text, time, unliftio, validity
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
    smos-data smos-report text time unliftio validity
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring dirforest genvalidity-sydtest monad-logger path
    path-io smos-data smos-data-gen smos-report smos-report-gen sydtest
    time
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-archive";
}
