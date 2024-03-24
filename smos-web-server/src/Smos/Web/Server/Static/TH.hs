module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Path.IO
import Smos.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
  cd <- resolveDir' "casts"
  cde <- doesDirExist cd
  let castsDir =
        if cde
          then "casts"
          else "../smos-docs-site/content/casts"

  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    [ remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico",
      remoteStatic "jquery.js" "https://code.jquery.com/jquery-3.7.1.min.js",
      remoteStatic "xterm.js" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js",
      remoteStatic "xterm.css" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css",
      remoteStatic "xterm-attach.js" "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.9.0/lib/xterm-addon-attach.min.js",
      remoteStatic "xterm-fit.js" "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/lib/xterm-addon-fit.min.js",
      remoteStatic "asciinema-player.js" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.min.js",
      remoteStatic "asciinema-player.css" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.css",
      remoteStatic "bulma-carousel.js" "https://cdn.jsdelivr.net/npm/bulma-carousel@4.0.24/dist/js/bulma-carousel.min.js",
      embedDir "assets",
      embedDirAt "casts" castsDir
    ]
