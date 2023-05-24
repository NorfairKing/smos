{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Smos.Web.Server.Handler.Admin
  ( getAdminPanelR,
    postAdminMigrateFilesR,
    getAdminUserR,
    postAdminUserSetSubscriptionR,
  )
where

import Smos.Data
import Smos.Web.Server.Handler.Import

getAdminPanelR :: Handler Html
getAdminPanelR = withAdminLogin $ \t -> do
  users <- runClientOrErr $ clientGetUsers t
  now <- liftIO getCurrentTime
  token <- genToken
  withNavBar $(widgetFile "admin/panel")

postAdminMigrateFilesR :: Handler Html
postAdminMigrateFilesR = withAdminLogin $ \t -> do
  NoContent <- runClientOrErr $ clientPostMigrateFiles t
  redirect AdminPanelR

getAdminUserR :: Username -> Handler Html
getAdminUserR username = withAdminLogin $ \t -> do
  user <- runClientOrErr $ clientGetUser t username
  mBookingSettings <- runClientOrErr $ clientGetBookingSettingsMaybe username
  let BookingSettings _ _ _ _ = undefined
  now <- liftIO getCurrentTime
  token <- genToken
  withNavBar $(widgetFile "admin/user")

postAdminUserSetSubscriptionR :: Username -> Handler Html
postAdminUserSetSubscriptionR username = withAdminLogin $ \t -> do
  endDate <- runInputPost $ ireq dayField "end-date"
  NoContent <- runClientOrErr $ clientPutUserSubscription t username $ UTCTime endDate 0
  redirect $ AdminUserR username
