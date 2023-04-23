{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutBookingSettings (servePutBookingSettings) where

import Smos.Server.Handler.Import

servePutBookingSettings :: AuthNCookie -> BookingSettings -> ServerHandler NoContent
servePutBookingSettings ac BookingSettings {..} = withUserId ac $ \uid -> do
  _ <-
    runDB $
      upsertBy
        (UniqueBookingConfigUser uid)
        BookingConfig
          { bookingConfigUser = uid,
            bookingConfigName = bookingSettingName,
            bookingConfigEmailAddress = bookingSettingEmailAddress,
            bookingConfigTimeZone = bookingSettingTimeZone
          }
        [ BookingConfigName =. bookingSettingName,
          BookingConfigEmailAddress =. bookingSettingEmailAddress,
          BookingConfigTimeZone =. bookingSettingTimeZone
        ]
  pure NoContent
