{ mkDerivation, aeson, autoexporter, base, containers, HaskellNet
, HaskellNet-SSL, lib, mime-mail, monad-logger, network
, opt-env-conf, path, path-io, safe-coloured-text
, safe-coloured-text-terminfo, smos-cli, smos-data, smos-data-gen
, smos-directory, smos-report, stache, sydtest, sydtest-discover
, text, time, unliftio
}:
mkDerivation {
  pname = "smos-jobhunt";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers HaskellNet HaskellNet-SSL mime-mail
    monad-logger network opt-env-conf path path-io safe-coloured-text
    safe-coloured-text-terminfo smos-cli smos-data smos-directory
    smos-report stache text time unliftio
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base monad-logger path path-io smos-data smos-data-gen sydtest time
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.mit;
  mainProgram = "smos-jobhunt";
}
