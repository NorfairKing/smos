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
    embedRemoteFileAt "asciinema-player.css" "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css",
    embedRemoteFileAt "bulma.css" "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.8.0/css/bulma.min.css",
    embedRemoteFileAt "font-awesome.css" "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
    embedRemoteFileAt "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico"
  ]
