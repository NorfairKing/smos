{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostStripeHook (servePostStripeHook) where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import Smos.Server.Handler.Import as Smos
import StripeClient as Stripe

servePostStripeHook :: JSON.Value -> ServerHandler NoContent
servePostStripeHook value = do
  logDebugNS "stripe-hook" $
    T.unlines
      [ "Got a request from Stripe:",
        TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
      ]
  case JSON.parseEither parseJSON value of
    Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event value from stripe: " <> T.pack err}
    Right event -> do
      let fullfillWith :: FromJSON a => (a -> ServerHandler ()) -> ServerHandler ()
          fullfillWith func = case parseEither parseJSON (toJSON (notificationEventDataObject (eventData event))) of
            Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event data in event with id " <> eventId event <> ": " <> T.pack err}
            Right r -> func r
      case eventType event of
        "checkout.session.completed" -> fullfillWith fullCheckoutSessionCompleted
        "invoice.paid" -> fullfillWith fullfillInvoicePaid
        "customer.subscription.created" -> fullfillWith fullfillSubscription
        "customer.subscription.updated" -> fullfillWith fullfillSubscription
        "customer.subscription.deleted" -> fullfillWith fullfillSubscription
        t -> do logInfoNS "stripe-hook" $ "Not handling event of type: " <> T.pack (show t)
  pure NoContent

-- | Record the stripe customer when a checkout session has been completed.
fullCheckoutSessionCompleted :: Stripe.Checkout'session -> ServerHandler ()
fullCheckoutSessionCompleted checkout = do
  logInfoNS "stripe-hook" $ T.pack $ unlines ["fulfilling completed checkout session:", ppShow checkout]
  customerId_ <- case checkout'sessionCustomer checkout of
    Nothing -> throwError err400 {errBody = "No customer in checkout"}
    Just (Checkout'sessionCustomer'Text cid) -> pure cid
    Just (Checkout'sessionCustomer'Customer c) -> pure $ customerId c
    Just (Checkout'sessionCustomer'DeletedCustomer _) -> throwError err400 {errBody = "Customer in checkout was a deleted customer."}

  let sessionId = checkout'sessionId checkout
  uid <- case checkout'sessionClientReferenceId checkout of
    Nothing -> throwError $ err404 {errBody = "Checkout did not contain a client reference id"}
    Just crid -> case parseUsernameWithError crid of
      Left err -> throwError $ err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse a username from the client reference id: " <> T.pack err}
      Right un -> do
        mUser <- runDB $ getBy $ UniqueUsername un
        case mUser of
          Nothing -> throwError $ err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No user found with username equal to the client reference id: " <> usernameText un}
          Just (Entity uid _) -> pure uid
  void $
    runDB $
      upsertBy
        (UniqueStripeCustomer uid customerId_)
        (StripeCustomer {stripeCustomerUser = uid, stripeCustomerCustomer = customerId_})
        [StripeCustomerCustomer =. customerId_]

-- | Record the stripe customer when an invoice has been paid, just in case this event comes first.
fullfillInvoicePaid :: Stripe.Invoice -> ServerHandler ()
fullfillInvoicePaid invoice = do
  logInfoNS "stripe-hook" $ T.pack $ unlines ["fulfilling invoice:", ppShow invoice]
  customerId_ <- case invoiceCustomer invoice of
    Nothing -> throwError err400 {errBody = "No customer found in invoice."}
    Just (InvoiceCustomer'Text cid) -> pure cid
    Just (InvoiceCustomer'Customer c) -> pure $ customerId c
    Just (InvoiceCustomer'DeletedCustomer _) -> throwError err400 {errBody = "Customer in checkout was a deleted customer."}

  -- Try to find the corresponding stripe customer so that we can figure out the user that this subscription belongs to
  mStripeCustomer <- runDB $ selectFirst [StripeCustomerCustomer ==. customerId_] [Desc StripeCustomerId]
  uid <- case mStripeCustomer of
    Nothing -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No stripe customer with this id found in the database: " <> customerId_}
    Just (Entity _ sc) -> pure $ stripeCustomerUser sc

  void $
    runDB $
      upsertBy
        (UniqueStripeCustomer uid customerId_)
        (StripeCustomer {stripeCustomerUser = uid, stripeCustomerCustomer = customerId_})
        [StripeCustomerCustomer =. customerId_]

-- | Update the subscription date when the subscription has been updated
fullfillSubscription :: Stripe.Subscription -> ServerHandler ()
fullfillSubscription subscription = do
  logInfoNS "stripe-hook" $ T.pack $ unlines ["fulfilling subscription:", ppShow subscription]
  customerId_ <- case subscriptionCustomer subscription of
    SubscriptionCustomer'Text cid -> pure cid
    SubscriptionCustomer'Customer c -> pure $ customerId c
    SubscriptionCustomer'DeletedCustomer _ -> throwError err400 {errBody = "Customer in subscription was a deleted customer."}

  -- Try to find the corresponding stripe customer so that we can figure out the user that this subscription belongs to
  mStripeCustomer <- runDB $ selectFirst [StripeCustomerCustomer ==. customerId_] [Desc StripeCustomerId]
  uid <- case mStripeCustomer of
    Nothing -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No stripe customer with this id found in the database: " <> customerId_}
    Just (Entity _ sc) -> pure $ stripeCustomerUser sc

  -- If the subscription has ended, use that date instead.
  let endtime = fromMaybe (subscriptionCurrentPeriodEnd subscription) (subscriptionEndedAt subscription)
  let end = posixSecondsToUTCTime $ fromIntegral endtime
  void $
    runDB $
      upsertBy
        (UniqueSubscriptionUser uid)
        (Smos.Subscription {subscriptionUser = uid, subscriptionEnd = end})
        [ SubscriptionEnd =. end
        ]
