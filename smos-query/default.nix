{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, conduit, containers, envparse, genvalidity
, genvalidity-containers, genvalidity-sydtest
, genvalidity-sydtest-aeson, lib, mtl, optparse-applicative, path
, path-io, pretty-relative-time, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-layout-gen
, safe-coloured-text-terminfo, smos-cli, smos-data, smos-report
, smos-report-gen, sydtest, sydtest-discover, text, time, tz
, validity, validity-path, yaml
}:
mkDerivation {
  pname = "smos-query";
  version = "0.8.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring conduit containers
    envparse mtl optparse-applicative path pretty-relative-time
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo smos-cli smos-data smos-report text
    time tz validity validity-path yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity genvalidity-containers
    genvalidity-sydtest genvalidity-sydtest-aeson path path-io
    safe-coloured-text-layout-gen smos-cli smos-report smos-report-gen
    sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-query";
}
