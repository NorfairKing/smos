{ mkDerivation, autodocodec, autoexporter, base, containers
, envparse, lib, optparse-applicative, path, path-io
, safe-coloured-text, safe-coloured-text-terminfo, smos-data
, smos-report, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "smos-sizing";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base containers envparse optparse-applicative path
    path-io safe-coloured-text safe-coloured-text-terminfo smos-data
    smos-report text
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-sizing";
}
