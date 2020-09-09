{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Playground
  ( getPlaygroundR,
    getPlaygroundInstanceR,
  )
where

import Path
import Servant.Client (showBaseUrl)
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosSession
import Smos.Web.Server.TUI
import Smos.Web.Server.Widget
import Yesod hiding (Header)
import Yesod.WebSockets

getPlaygroundR :: Handler Html
getPlaygroundR = do
  let tuiWidget = makeTuiWidget PlaygroundInstanceR
  mDocsUrl <- getsYesod appDocsBaseUrl
  withNavBar $ do
    setTitle "Smos Web TUI Playground"
    $(widgetFile "playground")

getPlaygroundInstanceR :: Handler ()
getPlaygroundInstanceR = do
  let relFile = [relfile|example.smos|]
  webSockets $ withPlaygroundSession relFile $ \instanceHandle -> communicateWithSmosInstance instanceHandle
