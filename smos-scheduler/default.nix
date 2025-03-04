{ mkDerivation, aeson, autodocodec, autodocodec-yaml, autoexporter
, base, bytestring, conduit, containers, cron, dirforest
, fuzzy-time, genvalidity, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, lib, megaparsec, monad-logger, mtl, opt-env-conf
, opt-env-conf-test, path, path-io, pretty-relative-time
, QuickCheck, safe-coloured-text, safe-coloured-text-terminfo
, smos-archive, smos-cli, smos-data, smos-directory
, smos-directory-gen, smos-report, smos-report-gen, sydtest
, sydtest-discover, text, time, tz, unliftio, validity
, validity-path, validity-text, yaml
}:
mkDerivation {
  pname = "smos-scheduler";
  version = "0.10.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring conduit containers cron
    fuzzy-time megaparsec monad-logger mtl opt-env-conf path path-io
    pretty-relative-time safe-coloured-text safe-coloured-text-terminfo
    smos-archive smos-cli smos-data smos-directory smos-report text
    time tz unliftio validity validity-path validity-text yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base containers cron dirforest
    genvalidity genvalidity-path genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time
    monad-logger mtl opt-env-conf-test path path-io QuickCheck
    smos-archive smos-cli smos-data smos-directory smos-directory-gen
    smos-report smos-report-gen sydtest text time tz
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-scheduler";
}
