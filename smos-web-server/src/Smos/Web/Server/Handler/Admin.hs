{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Admin where

import Servant.Client
import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Yesod hiding (Header)

getAdminR :: Handler Html
getAdminR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  withNavBar $ do
    $(widgetFile "admin/panel")
