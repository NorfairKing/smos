module Smos.Server.Handler.GetBookingSettings (serveGetBookingSettings) where

import Smos.Server.Handler.Import

serveGetBookingSettings :: AuthNCookie -> ServerHandler BookingSettings
serveGetBookingSettings ac = withUserId ac $ \uid ->
  undefined
