{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Admin where

import Smos.Web.Server.Handler.Import

getAdminPanelR :: Handler Html
getAdminPanelR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  now <- liftIO getCurrentTime
  withNavBar $ do
    $(widgetFile "admin/panel")
