{ mkDerivation, autodocodec, base, conduit, containers, envparse
, filepath, hashable, lib, monad-logger, opt-env-conf
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, pretty-relative-time, pretty-show, process
, smos-cli, smos-data, smos-directory, text, time
}:
mkDerivation {
  pname = "smos-notify";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    autodocodec base conduit containers envparse filepath hashable
    monad-logger opt-env-conf optparse-applicative path path-io
    persistent persistent-sqlite pretty-relative-time pretty-show
    process smos-cli smos-data smos-directory text time
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "smos-notify";
}
