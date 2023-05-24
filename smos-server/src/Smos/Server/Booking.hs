{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Booking (getUserBookingSettings, bookingFilePath) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Smos.API
import Smos.Server.DB
import Smos.Server.Env

getUserBookingSettings :: UserId -> ServerHandler (Maybe BookingSettings)
getUserBookingSettings uid = do
  mServerFile <- runDB $ getBy $ UniqueServerFilePath uid bookingFilePath
  case mServerFile of
    Nothing -> pure Nothing
    Just (Entity _ ServerFile {..}) -> case Yaml.decodeEither' serverFileContents of
      Left err -> do
        logWarnN $
          T.pack $
            unlines
              [ "Found a booking config, but could not parse it:",
                show err
              ]
        pure Nothing
      Right bs -> pure $ Just bs
