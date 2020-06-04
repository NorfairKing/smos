{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Yesod
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addScript $ StaticR smos_web_server_front_js
  $(widgetFile "home")
