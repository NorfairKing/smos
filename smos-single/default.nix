{ mkDerivation, base, lib, opt-env-conf, opt-env-conf-test, path
, path-io, smos-cli, smos-data, smos-directory, sydtest
, sydtest-discover, text, time
}:
mkDerivation {
  pname = "smos-single";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base opt-env-conf path path-io smos-cli smos-data smos-directory
    text time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-single";
}
