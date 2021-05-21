{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetMonetisation
  ( serveGetMonetisation,
  )
where

import Smos.Server.Handler.Import

serveGetMonetisation :: ServerHandler (Maybe Monetisation)
serveGetMonetisation = do
  mm <- asks serverEnvMonetisationSettings
  forM mm $ \MonetisationSettings {..} ->
    pure
      -- We don't send over the secret key on purpose.
      Monetisation
        { monetisationStripePublishableKey = monetisationSetStripePublishableKey,
          monetisationStripePlan = monetisationSetStripePlan
        }
