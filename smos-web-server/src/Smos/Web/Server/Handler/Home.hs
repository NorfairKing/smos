{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")
