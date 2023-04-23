module Smos.Server.Handler.PostBooking (servePostBooking) where

import Smos.Server.Handler.Import

servePostBooking :: Username -> ServerHandler NoContent
servePostBooking username = withUsernameId username $ \uid -> do
  pure NoContent
