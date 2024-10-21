{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, conduit, containers, genvalidity-sydtest
, IntervalMap, lib, mtl, opt-env-conf, path, path-io
, pretty-relative-time, safe-coloured-text
, safe-coloured-text-terminfo, smos-cli, smos-data, smos-directory
, smos-directory-gen, smos-report, sydtest, sydtest-discover, text
, time, tz, validity-path, yaml
}:
mkDerivation {
  pname = "smos-query";
  version = "0.11.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring conduit containers
    IntervalMap mtl opt-env-conf path pretty-relative-time
    safe-coloured-text safe-coloured-text-terminfo smos-cli smos-data
    smos-directory smos-report text time tz validity-path yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest path path-io smos-cli
    smos-directory smos-directory-gen smos-report sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-query";
}
