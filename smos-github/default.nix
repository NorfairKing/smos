{ mkDerivation, aeson, async, autoexporter, base, conduit
, containers, genvalidity, genvalidity-sydtest, genvalidity-text
, genvalidity-time, github, lib, network-uri, opt-env-conf, path
, path-io, safe-coloured-text, safe-coloured-text-terminfo
, smos-cli, smos-data, smos-data-gen, smos-directory, sydtest
, sydtest-discover, text, time, validity, validity-path
, validity-text
}:
mkDerivation {
  pname = "smos-github";
  version = "0.7.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base conduit containers github network-uri opt-env-conf
    path path-io safe-coloured-text safe-coloured-text-terminfo
    smos-cli smos-data smos-directory text time validity validity-text
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest genvalidity-text
    genvalidity-time github path smos-data-gen sydtest time
    validity-path
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-github";
}
