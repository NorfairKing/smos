module Smos.Server.Handler.PutBookingSettings (servePutBookingSettings) where

import Smos.Server.Handler.Import

servePutBookingSettings :: AuthNCookie -> BookingSettings -> ServerHandler NoContent
servePutBookingSettings ac bs = withUserId ac $ \uid ->
  undefined
