{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSettings (serveGetBookingSettings) where

import Smos.Server.Handler.Import

serveGetBookingSettings :: Username -> ServerHandler BookingSettings
serveGetBookingSettings username = withUsernameId username $ \uid -> do
  mBookingConfig <- runDB $ getBy $ UniqueBookingConfigUser uid
  case mBookingConfig of
    Nothing -> throwError err404
    Just (Entity _ BookingConfig {..}) -> do
      let bookingSettingTimeZone = bookingConfigTimeZone
      pure BookingSettings {..}
