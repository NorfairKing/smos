{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosServer (getSmosServerR) where

import Smos.Docs.Site.Handler.Import
import Smos.Server.OptParse as Server

getSmosServerR :: Handler Html
getSmosServerR = makeSettingsPage @Server.Settings "smos-server"
