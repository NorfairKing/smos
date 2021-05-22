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
  let username = authNCookieUsername ac
  mMonetisationSettings <- asks serverEnvMonetisationSettings
  case mMonetisationSettings of
    Nothing -> throwError err404
    Just MonetisationSettings {..} -> do
      let config =
            Stripe.defaultConfiguration
              { configSecurityScheme = bearerAuthenticationSecurityScheme monetisationSetStripeSecretKey
              }
      withUserId ac $ \uid -> do
        -- Get or create the stripe customer
        mStripeCustomer <- runDB $ selectFirst [StripeCustomerUser ==. uid] [Desc StripeCustomerId]
        Entity _ stripeCustomer <- case mStripeCustomer of
          Just sce -> pure sce
          Nothing -> do
            -- TODO add in the email address too
            let postCustomersRequest = mkPostCustomersRequestBody {postCustomersRequestBodyDescription = Just $ usernameText username}
            resp <- liftIO $ runWithConfiguration config $ postCustomers $ Just postCustomersRequest
            case responseBody resp of
              PostCustomersResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:\n" <> T.pack err}
              PostCustomersResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
              PostCustomersResponse200 Customer {..} ->
                -- Keep track of it in our database for later
                runDB $
                  upsertBy
                    (UniqueStripeCustomer uid customerId)
                    (StripeCustomer {stripeCustomerUser = uid, stripeCustomerCustomer = customerId})
                    [StripeCustomerCustomer =. customerId]

        -- Make the request to create a checkout session
        let lineItems =
              [ mkPostCheckoutSessionsRequestBodyLineItems'
                  { postCheckoutSessionsRequestBodyLineItems'Quantity = Just 1,
                    postCheckoutSessionsRequestBodyLineItems'Price = Just monetisationSetStripePrice
                  }
              ]
            methodTypes = [PostCheckoutSessionsRequestBodyPaymentMethodTypes'EnumCard]
            successUrl = initiateStripeCheckoutSessionSuccessUrl iscs
            cancelUrl = initiateStripeCheckoutSessionCanceledUrl iscs
        let request =
              (mkPostCheckoutSessionsRequestBody cancelUrl methodTypes successUrl)
                { postCheckoutSessionsRequestBodyCustomer = Just $ stripeCustomerCustomer stripeCustomer,
                  postCheckoutSessionsRequestBodyClientReferenceId = Just $ usernameText username,
                  postCheckoutSessionsRequestBodyLineItems = Just lineItems,
                  postCheckoutSessionsRequestBodyMode = Just PostCheckoutSessionsRequestBodyMode'EnumSubscription
                }

        -- Actually perform the request
        resp <- liftIO $ runWithConfiguration config $ postCheckoutSessions request :: ServerHandler (HTTP.Response PostCheckoutSessionsResponse)
        case responseBody resp of
          PostCheckoutSessionsResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:" <> T.pack err}
          PostCheckoutSessionsResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
          PostCheckoutSessionsResponse200 session -> do
            pure $
              InitiatedCheckoutSession
                { initiatedCheckoutSessionId = checkout'sessionId session,
                  initiatedCheckoutSessionCustomerId = case checkout'sessionCustomer session of
                    Nothing -> Nothing
                    Just (Checkout'sessionCustomer'Text cid) -> Just cid
                    Just (Checkout'sessionCustomer'Customer c) -> Just (customerId c)
                    Just (Checkout'sessionCustomer'DeletedCustomer _) -> Nothing
                }
