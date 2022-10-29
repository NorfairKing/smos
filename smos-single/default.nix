{ mkDerivation, autodocodec, base, envparse, lib
, optparse-applicative, path, path-io, smos-data, smos-report, text
, time
}:
mkDerivation {
  pname = "smos-single";
  version = "0.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base envparse optparse-applicative path path-io
    smos-data smos-report text time
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
}
