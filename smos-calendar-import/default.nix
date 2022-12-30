{ mkDerivation, aeson, async, autodocodec, autodocodec-yaml, base
, bytestring, case-insensitive, containers, deepseq, envparse
, genvalidity, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-text, http-client, http-client-tls, ical, ical-gen
, ical-recurrence, ical-recurrence-gen, lib, mtl, network-uri
, optparse-applicative, path, path-io, QuickCheck, smos-data
, smos-data-gen, smos-report, sydtest, sydtest-discover, text, time
, validity, validity-text, yaml
}:
mkDerivation {
  pname = "smos-calendar-import";
  version = "0.6.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec base bytestring case-insensitive containers
    envparse http-client http-client-tls ical ical-recurrence mtl
    network-uri optparse-applicative path path-io smos-data smos-report
    text time validity validity-text yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring containers deepseq
    genvalidity genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text ical ical-gen ical-recurrence ical-recurrence-gen
    path path-io QuickCheck smos-data smos-data-gen sydtest yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-calendar-import";
}
