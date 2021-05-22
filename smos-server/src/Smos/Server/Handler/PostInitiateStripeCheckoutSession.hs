{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostInitiateStripeCheckoutSession
  ( servePostInitiateStripeCheckoutSession,
  )
where

import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import Smos.Server.Handler.Import
import StripeClient as Stripe

servePostInitiateStripeCheckoutSession :: AuthNCookie -> InitiateStripeCheckoutSession -> ServerHandler InitiatedCheckoutSession
servePostInitiateStripeCheckoutSession ac iscs = do
  mMonetisationSettings <- asks serverEnvMonetisationSettings
  case mMonetisationSettings of
    Nothing -> throwError err404
    Just MonetisationSettings {..} ->
      withUserId ac $ \uid -> do
        mStripeCustomer <- runDB $ selectFirst [StripeCustomerUser ==. uid] [Desc StripeCustomerId]
        -- TODO get or create customer
        let lineItems =
              [ mkPostCheckoutSessionsRequestBodyLineItems'
                  { postCheckoutSessionsRequestBodyLineItems'Quantity = Just 1,
                    postCheckoutSessionsRequestBodyLineItems'Price = Just monetisationSetStripePrice
                  }
              ]
            methodTypes = [PostCheckoutSessionsRequestBodyPaymentMethodTypes'EnumCard]
            successUrl = initiateStripeCheckoutSessionSuccessUrl iscs
            cancelUrl = initiateStripeCheckoutSessionCanceledUrl iscs
            mCustomerId = stripeCustomerCustomer . entityVal <$> mStripeCustomer
        let request =
              (mkPostCheckoutSessionsRequestBody cancelUrl methodTypes successUrl)
                { postCheckoutSessionsRequestBodyCustomer = mCustomerId,
                  postCheckoutSessionsRequestBodyClientReferenceId = Just $ usernameText (authNCookieUsername ac),
                  postCheckoutSessionsRequestBodyLineItems = Just lineItems,
                  postCheckoutSessionsRequestBodyMode = Just PostCheckoutSessionsRequestBodyMode'EnumSubscription
                }
        let config =
              Stripe.defaultConfiguration
                { configSecurityScheme = bearerAuthenticationSecurityScheme monetisationSetStripeSecretKey
                }

        resp <- liftIO $ runWithConfiguration config $ postCheckoutSessions request :: ServerHandler (HTTP.Response PostCheckoutSessionsResponse)
        case responseBody resp of
          PostCheckoutSessionsResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:" <> T.pack err}
          PostCheckoutSessionsResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
          PostCheckoutSessionsResponse200 session ->
            pure $
              InitiatedCheckoutSession
                { initiatedCheckoutSessionId = checkout'sessionId session,
                  initiatedCheckoutSessionCustomerId = case checkout'sessionCustomer session of
                    Nothing -> Nothing
                    Just (Checkout'sessionCustomer'Text cid) -> Just cid
                    Just (Checkout'sessionCustomer'Customer c) -> Just (customerId c)
                    Just (Checkout'sessionCustomer'DeletedCustomer _) -> Nothing
                }
