{ mkDerivation, aeson, autodocodec, autodocodec-yaml, autoexporter
, base, base64-bytestring, bytestring, conduit, containers, cron
, dirforest, fuzzy-time, genvalidity, genvalidity-bytestring
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, lib, megaparsec, mtl, opt-env-conf, opt-env-conf-test, path
, path-io, pretty-relative-time, QuickCheck, safe
, safe-coloured-text, safe-coloured-text-terminfo, smos-cli
, smos-data, smos-data-gen, smos-directory, smos-directory-gen
, smos-report, smos-report-gen, sydtest, sydtest-discover, text
, time, tz, unliftio, validity, validity-path, validity-text, yaml
}:
mkDerivation {
  pname = "smos-scheduler";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base base64-bytestring bytestring conduit
    containers cron fuzzy-time megaparsec mtl opt-env-conf path path-io
    pretty-relative-time safe safe-coloured-text
    safe-coloured-text-terminfo smos-cli smos-data smos-directory
    smos-report text time tz unliftio validity validity-path
    validity-text yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base containers cron dirforest
    genvalidity genvalidity-bytestring genvalidity-containers
    genvalidity-path genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text genvalidity-time mtl opt-env-conf-test path
    path-io QuickCheck smos-cli smos-data smos-data-gen smos-directory
    smos-directory-gen smos-report smos-report-gen sydtest text time tz
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-scheduler";
}
