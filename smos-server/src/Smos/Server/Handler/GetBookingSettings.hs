module Smos.Server.Handler.GetBookingSettings (serveGetBookingSettings) where

import Smos.Server.Booking
import Smos.Server.Handler.Import

serveGetBookingSettings :: Username -> ServerHandler BookingSettings
serveGetBookingSettings username = withUsernameId username $ \uid -> do
  mBookingSettings <- getUserBookingSettings uid
  case mBookingSettings of
    Nothing -> throwError err404
    Just bs -> pure bs
