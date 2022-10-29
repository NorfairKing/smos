{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, cmark-gfm, containers
, data-default, envparse, filepath, fsnotify, lib
, optparse-applicative, path, path-io, raw-strings-qq, semver
, shakespeare, smos, smos-archive, smos-calendar-import
, smos-client, smos-data, smos-github, smos-notify, smos-query
, smos-report, smos-scheduler, smos-server, smos-single
, smos-sync-client, smos-web-server, smos-web-style
, template-haskell, template-haskell-reload, text
, th-lift-instances, time, typed-process, wai-extra, warp, yaml
, yesod, yesod-autoreload, yesod-sitemap, yesod-static
, yesod-static-remote
}:
mkDerivation {
  pname = "smos-docs-site";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    cmark-gfm containers data-default envparse filepath fsnotify
    optparse-applicative path path-io raw-strings-qq semver shakespeare
    smos smos-archive smos-calendar-import smos-client smos-data
    smos-github smos-notify smos-query smos-report smos-scheduler
    smos-server smos-single smos-sync-client smos-web-server
    smos-web-style template-haskell template-haskell-reload text
    th-lift-instances time typed-process wai-extra warp yaml yesod
    yesod-autoreload yesod-sitemap yesod-static yesod-static-remote
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/smos#readme";
  license = lib.licenses.mit;
}
