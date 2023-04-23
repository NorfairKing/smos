module Smos.Server.Handler.DeleteBookingSettings (serveDeleteBookingSettings) where

import Smos.Server.Handler.Import

serveDeleteBookingSettings :: AuthNCookie -> ServerHandler NoContent
serveDeleteBookingSettings ac = withUserId ac $ \uid -> do
  runDB $ deleteBy $ UniqueBookingConfigUser uid
  pure NoContent
