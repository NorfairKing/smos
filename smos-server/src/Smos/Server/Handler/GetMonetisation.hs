{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetMonetisation
  ( serveGetMonetisation,
  )
where

import Control.Concurrent
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Smos.Server.Handler.Import
import StripeClient as Stripe

serveGetMonetisation :: ServerHandler (Maybe Monetisation)
serveGetMonetisation = do
  mm <- asks serverEnvMonetisationSettings
  forM mm $ \ms@MonetisationSettings {..} -> do
    price <- getStripePrice ms
    amount <- case priceUnitAmount price of
      Nothing -> throwError err500 {errBody = "Error while calling stripe: The price had no unit amount."}
      Just ua -> pure ua
    pure
      -- We don't send over the secret key, on purpose.
      Monetisation
        { monetisationStripePublishableKey = monetisationSetStripePublishableKey,
          monetisationStripePriceCurrency = Stripe.priceCurrency price,
          monetisationStripePricePerYear = amount
        }

getStripePrice :: MonetisationSettings -> ServerHandler Price
getStripePrice ms = do
  priceVar <- asks serverEnvPriceCache
  mPrice <- liftIO $ tryReadMVar priceVar
  case mPrice of
    -- Already cached, just return it
    Just price -> do
      logDebugN "Stripe price was already cached."
      pure price
    -- Not cached yet
    Nothing -> do
      logDebugN "Stripe price was not in cache yet, getting it from Stripe now."
      -- Get it from stripe
      price <- getStripePriceFromStripe ms
      -- Put in the cache for later
      liftIO $ putMVar priceVar price
      pure price

getStripePriceFromStripe :: MonetisationSettings -> ServerHandler Price
getStripePriceFromStripe MonetisationSettings {..} = do
  let config =
        Stripe.defaultConfiguration
          { configSecurityScheme = bearerAuthenticationSecurityScheme monetisationSetStripeSecretKey
          }
  let req = mkGetPricesPriceParameters monetisationSetStripePrice
  resp <- liftIO $ runWithConfiguration config $ getPricesPrice req
  case HTTP.responseBody resp of
    GetPricesPriceResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:\n" <> T.pack err}
    GetPricesPriceResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
    GetPricesPriceResponse200 price -> do
      logDebugN $ T.pack $ "Received this price from Stripe:\n" <> ppShow price
      pure price
