{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Assets where

import Smos.Docs.Site.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "assets"
  ( let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
     in [ embedDir "content/assets",
          remoteStatic "asciinema-player.js" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.min.js",
          remoteStatic "asciinema-player.css" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.css",
          remoteStatic "font-awesome.css" "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
          remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico"
        ]
  )
