module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Smos.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    [ remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico",
      remoteStatic "jquery.js" "https://code.jquery.com/jquery-3.3.1.min.js",
      remoteStatic "xterm.js" "https://cdn.jsdelivr.net/npm/xterm@4.8.1/lib/xterm.min.js",
      remoteStatic "xterm.css" "https://cdn.jsdelivr.net/npm/xterm@4.8.1/css/xterm.css",
      remoteStatic "xterm-attach.js" "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.6.0/lib/xterm-addon-attach.min.js",
      remoteStatic "xterm-fit.js" "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.4.0/lib/xterm-addon-fit.min.js",
      embedDir "assets"
    ]
