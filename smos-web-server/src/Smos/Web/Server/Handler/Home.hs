{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod hiding (Header)
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  withNavBar $(widgetFile "home")
