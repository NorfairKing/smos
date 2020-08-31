module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Smos.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic =
  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    [ embedRemoteFileAt "jquery.js" "https://code.jquery.com/jquery-3.3.1.min.js",
      embedRemoteFileAt "xterm.js" "https://cdn.jsdelivr.net/npm/xterm@4.8.1/lib/xterm.min.js",
      embedRemoteFileAt "xterm.css" "https://cdn.jsdelivr.net/npm/xterm@4.8.1/css/xterm.css",
      embedRemoteFileAt "xterm-attach.js" "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.6.0/lib/xterm-addon-attach.min.js",
      embedRemoteFileAt "xterm-fit.js" "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.4.0/lib/xterm-addon-fit.min.js"
    ]
