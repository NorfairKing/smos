{ mkDerivation, aeson, autodocodec, autoexporter, base, containers
, envparse, HaskellNet, HaskellNet-SSL, lib, mime-mail
, monad-logger, network, optparse-applicative, path, path-io
, safe-coloured-text, safe-coloured-text-terminfo, smos-archive
, smos-cli, smos-data, smos-data-gen, smos-report, stache, sydtest
, sydtest-discover, text, time, unliftio
}:
mkDerivation {
  pname = "smos-jobhunt";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base containers envparse HaskellNet
    HaskellNet-SSL mime-mail monad-logger network optparse-applicative
    path path-io safe-coloured-text safe-coloured-text-terminfo
    smos-archive smos-cli smos-data smos-report stache text time
    unliftio
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
