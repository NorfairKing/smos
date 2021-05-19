{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Admin where

import Smos.Web.Server.Handler.Import

getAdminPanelR :: Handler Html
getAdminPanelR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  now <- liftIO getCurrentTime
  withNavBar $(widgetFile "admin/panel")

getAdminUserR :: Username -> Handler Html
getAdminUserR username = withAdminLogin $ \t -> do
  user <- runClientOrErr $ clientGetUser t username
  now <- liftIO getCurrentTime
  withNavBar $(widgetFile "admin/user")
