{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Booking (getUserBookingSettings, bookingFilePath) where

import qualified Data.Yaml as Yaml
import Path
import Smos.API
import Smos.Server.DB
import Smos.Server.Env

getUserBookingSettings :: UserId -> ServerHandler (Maybe BookingSettings)
getUserBookingSettings uid = do
  mServerFile <- runDB $ getBy $ UniqueServerFilePath uid bookingFilePath
  pure $ do
    Entity _ ServerFile {..} <- mServerFile
    case Yaml.decodeEither' serverFileContents of
      Left _ -> Nothing
      Right bs -> Just bs

bookingFilePath :: Path Rel File
bookingFilePath = [relfile|server-config/booking.yaml|]
