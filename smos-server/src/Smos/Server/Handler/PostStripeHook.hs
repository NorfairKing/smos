{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostStripeHook (servePostStripeHook) where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import Smos.Server.Handler.Import
import StripeClient as Strype

servePostStripeHook :: JSON.Value -> ServerHandler NoContent
servePostStripeHook value = do
  logDebugNS "stripe-hook" $
    T.unlines
      [ "Got a request from Stripe:",
        TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
      ]
  case JSON.parseEither parseJSON value of
    Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event value from stripe: " <> T.pack err}
    Right event -> case eventType event of
      "checkout.session.completed" ->
        case parseEither parseJSON (toJSON (notificationEventDataObject (eventData event))) of
          Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event data as checkout in event with id " <> eventId event <> ": " <> T.pack err}
          Right r -> fullCheckoutSessionCompleted r
      "invoice.paid" ->
        case parseEither parseJSON (toJSON (notificationEventDataObject (eventData event))) of
          Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event data as invoice in event with id " <> eventId event <> ": " <> T.pack err}
          Right r -> fullfillInvoicePaid r
      t -> do
        logInfoNS "stripe-hook" $ "Not handling event of type: " <> T.pack (show t)
  pure NoContent

fullCheckoutSessionCompleted :: Checkout'session -> ServerHandler ()
fullCheckoutSessionCompleted checkout = do
  customerId <- case checkout'sessionCustomer checkout of
    Nothing -> throwError err400 {errBody = "No customer in checkout"}
    Just (Checkout'sessionCustomer'Text cid) -> pure cid
    Just (Checkout'sessionCustomer'Customer c) -> pure $ customerId c
    Just (Checkout'sessionCustomer'DeletedCustomer _) -> throwError err400 {errBody = "Customer in checkout was a deleted customer."}

  uid <- case checkout'sessionClientReferenceId checkout of
    Nothing -> throwError err400 {errBody = "Checkout did not contain a client reference id"}
    Just crid -> case parseUsernameWithError crid of
      Left err -> throwError err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse a username from the client reference id:" <> T.pack err}
      Right un -> do
        mUser <- runDB $ getBy $ UniqueUsername un
        case mUser of
          Nothing -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No user found with username equal to the client reference id:" <> usernameText un}
          Just (Entity uid _) -> pure uid
  void $
    runDB $
      upsertBy
        (UniqueStripeCustomer uid customerId)
        (StripeCustomer {stripeCustomerUser = uid, stripeCustomerCustomer = customerId})
        [StripeCustomerCustomer =. customerId]

fullfillInvoicePaid :: Invoice -> ServerHandler ()
fullfillInvoicePaid invoice = do
  logDebugNS "stripe-hook" $ T.pack $ unlines ["fulfilling invoice:", ppShow invoice]
  customerId <- case invoiceCustomer invoice of
    Nothing -> throwError err400 {errBody = "No customer found in invoice."}
    Just (InvoiceCustomer'Text cid) -> pure cid
    Just (InvoiceCustomer'Customer c) -> pure $ customerId c
    Just (InvoiceCustomer'DeletedCustomer _) -> throwError err400 {errBody = "Customer in checkout was a deleted customer."}
  -- Get the newest customer with this id.
  -- If there are duplicates (which there can be), then we _probably_ want the latest?
  mStripeCustomer <- runDB $ selectFirst [StripeCustomerCustomer ==. customerId] [Desc StripeCustomerId]
  uid <- case mStripeCustomer of
    Nothing -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No stripe customer with this id found in the database: " <> customerId}
    Just (Entity _ sc) -> pure $ stripeCustomerUser sc
  let end = posixSecondsToUTCTime $ fromIntegral $ invoicePeriodEnd invoice
  void $
    runDB $
      upsertBy
        (UniqueSubscriptionUser uid)
        (Subscription {subscriptionUser = uid, subscriptionEnd = end})
        [ SubscriptionEnd =. end
        ]
