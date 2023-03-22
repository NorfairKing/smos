{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, conduit, containers, envparse
, genvalidity-sydtest, lib, mtl, optparse-applicative, path
, path-io, pretty-relative-time, safe-coloured-text
, safe-coloured-text-terminfo, smos-cli, smos-data, smos-directory
, smos-directory-gen, smos-report, sydtest, sydtest-discover, text
, time, tz, validity, validity-path, yaml
}:
mkDerivation {
  pname = "smos-query";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring conduit containers
    envparse mtl optparse-applicative path pretty-relative-time
    safe-coloured-text safe-coloured-text-terminfo smos-cli smos-data
    smos-directory smos-report text time tz validity validity-path yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity-sydtest path path-io smos-directory-gen
    smos-report sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-query";
}
