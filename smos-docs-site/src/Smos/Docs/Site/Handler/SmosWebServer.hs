{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosWebServer (getSmosWebServerR) where

import Smos.Docs.Site.Handler.Import
import Smos.Web.Server.OptParse as WebServer

getSmosWebServerR :: Handler Html
getSmosWebServerR = makeSettingsPage @WebServer.Settings "smos-web-server"
