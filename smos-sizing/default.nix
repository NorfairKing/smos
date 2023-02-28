{ mkDerivation, autodocodec, autoexporter, base, containers
, envparse, genvalidity-sydtest, lib, optparse-applicative, path
, path-io, safe-coloured-text, safe-coloured-text-terminfo
, smos-data, smos-data-gen, smos-report, sydtest, sydtest-discover
, text
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
  testHaskellDepends = [
    base genvalidity-sydtest smos-data-gen sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-sizing";
}
