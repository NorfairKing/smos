{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Admin where

import Data.Time
import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Text.Time.Pretty
import Yesod hiding (Header)

getAdminPanelR :: Handler Html
getAdminPanelR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  now <- liftIO getCurrentTime
  withNavBar $ do
    $(widgetFile "admin/panel")
