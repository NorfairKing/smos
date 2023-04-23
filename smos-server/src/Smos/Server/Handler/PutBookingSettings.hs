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
            bookingConfigTimeZone = bookingSettingTimeZone
          }
        [ BookingConfigTimeZone =. bookingSettingTimeZone
        ]
  pure NoContent
