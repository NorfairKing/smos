{ mkDerivation, autoexporter, base, bytestring, conduit, containers
, dirforest, exceptions, filepath, genvalidity-sydtest, lib
, monad-logger, mtl, opt-env-conf, path, path-io, smos-cli
, smos-data, smos-data-gen, smos-directory, smos-directory-gen
, smos-report, sydtest, sydtest-discover, text, time, unliftio
}:
mkDerivation {
  pname = "smos-archive";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring conduit containers exceptions filepath monad-logger
    mtl opt-env-conf path path-io smos-cli smos-data smos-directory
    smos-report text time unliftio
  ];
  libraryToolDepends = [ autoexporter ];
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
