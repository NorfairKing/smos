{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, case-insensitive, conformance, conformance-gen
, containers, deepseq, genvalidity, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, http-client
, http-client-tls, ical, ical-gen, ical-recurrence
, ical-recurrence-gen, lib, monad-logger, network-uri, opt-env-conf
, path, path-io, QuickCheck, smos-cli, smos-data, smos-data-gen
, smos-directory, sydtest, sydtest-discover, text, time, unliftio
, validity, validity-text, yaml
}:
mkDerivation {
  pname = "smos-calendar-import";
  version = "0.9.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring case-insensitive conformance
    containers http-client http-client-tls ical ical-recurrence
    monad-logger network-uri opt-env-conf path path-io smos-cli
    smos-data smos-directory text time unliftio validity validity-text
    yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring conformance-gen
    containers deepseq genvalidity genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text ical ical-gen
    ical-recurrence ical-recurrence-gen path path-io QuickCheck
    smos-data smos-data-gen sydtest yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-calendar-import";
}
