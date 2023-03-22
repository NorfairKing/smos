{ mkDerivation, autodocodec, base, envparse, lib
, optparse-applicative, path, path-io, smos-cli, smos-data
, smos-directory, text, time
}:
mkDerivation {
  pname = "smos-single";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base envparse optparse-applicative path path-io
    smos-cli smos-data smos-directory text time
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-single";
}
