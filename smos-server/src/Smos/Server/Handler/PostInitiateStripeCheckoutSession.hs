{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostInitiateStripeCheckoutSession
  ( servePostInitiateStripeCheckoutSession,
  )
where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import Network.HTTP.Client as HTTP
import Paths_smos_server
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
        let metadata =
              KM.fromList
                [ ("product", "smos"),
                  ("smos-server-version", toJSON $ showVersion version)
                ]
        Entity _ stripeCustomer <- case mStripeCustomer of
          Just sce -> pure sce
          Nothing -> do
            let postCustomersRequest =
                  mkPostCustomersRequestBody
                    { postCustomersRequestBodyDescription = Just $ usernameText username,
                      postCustomersRequestBodyMetadata =
                        Just $
                          PostCustomersRequestBodyMetadata'Object metadata
                    }
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
            successUrl = initiateStripeCheckoutSessionSuccessUrl iscs
            cancelUrl = initiateStripeCheckoutSessionCanceledUrl iscs
        let request =
              (mkPostCheckoutSessionsRequestBody cancelUrl successUrl)
                { postCheckoutSessionsRequestBodyCustomer = Just $ stripeCustomerCustomer stripeCustomer,
                  postCheckoutSessionsRequestBodyClientReferenceId = Just $ usernameText username,
                  postCheckoutSessionsRequestBodyLineItems = Just lineItems,
                  postCheckoutSessionsRequestBodyMode = Just PostCheckoutSessionsRequestBodyMode'EnumSubscription,
                  postCheckoutSessionsRequestBodyMetadata = Just metadata,
                  postCheckoutSessionsRequestBodySubscriptionData =
                    Just $
                      mkPostCheckoutSessionsRequestBodySubscriptionData'
                        { postCheckoutSessionsRequestBodySubscriptionData'Metadata = Just metadata
                        }
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
                    Just (NonNull (Checkout'sessionCustomer'NonNullableText cid)) -> Just cid
                    Just (NonNull (Checkout'sessionCustomer'NonNullableCustomer c)) -> Just (customerId c)
                    Just (NonNull (Checkout'sessionCustomer'NonNullableDeletedCustomer _)) -> Nothing
                    _ -> Nothing
                }
