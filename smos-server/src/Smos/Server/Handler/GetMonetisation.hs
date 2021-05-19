module Smos.Server.Handler.GetMonetisation
  ( serveGetMonetisation,
  )
where

import Smos.Server.Handler.Import

serveGetMonetisation :: ServerHandler (Maybe Monetisation)
serveGetMonetisation = do
  mm <- asks serverEnvMonetisationSettings
  forM mm $ \_ -> pure Monetisation
