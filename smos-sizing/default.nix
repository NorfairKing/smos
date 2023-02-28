{ mkDerivation, autodocodec, autoexporter, base, envparse, lib
, optparse-applicative, path, path-io, smos-data, smos-report
, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "smos-sizing";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base envparse optparse-applicative path path-io
    smos-data smos-report
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-sizing";
}
