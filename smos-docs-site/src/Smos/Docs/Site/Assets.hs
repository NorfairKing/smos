{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Assets where

import Smos.Docs.Site.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "assets"
  [ embedDir "content/assets",
    embedRemoteFileAt "asciinema-player.js" "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js",
    embedRemoteFileAt "asciinema-player.css" "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css"
  ]
