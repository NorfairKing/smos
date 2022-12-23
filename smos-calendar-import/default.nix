{ mkDerivation, aeson, async, autodocodec, autodocodec-yaml, base
, bytestring, containers, data-default, dlist, envparse
, genvalidity, genvalidity-containers, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, http-client, http-client-tls, ical, ical-gen, ical-recurrence
, ical-recurrence-gen, lib, mtl, network-uri, optparse-applicative
, path, path-io, QuickCheck, safe, smos-data, smos-data-gen
, smos-report, sydtest, sydtest-discover, text, time, time-compat
, validity, validity-containers, validity-text, validity-time, yaml
}:
mkDerivation {
  pname = "smos-calendar-import";
  version = "0.6.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec base bytestring containers data-default
    dlist envparse http-client http-client-tls ical ical-recurrence mtl
    network-uri optparse-applicative path path-io safe smos-data
    smos-report text time time-compat validity validity-containers
    validity-text validity-time yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring containers
    data-default genvalidity genvalidity-containers genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time ical
    ical-gen ical-recurrence-gen path path-io QuickCheck safe smos-data
    smos-data-gen sydtest text time yaml
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-calendar-import";
}
