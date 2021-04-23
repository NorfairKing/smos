{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Admin where

import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod hiding (Header)

getAdminPanelR :: Handler Html
getAdminPanelR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  withNavBar $ do
    $(widgetFile "admin/panel")
