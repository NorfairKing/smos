{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Checkout
  ( getCheckoutR,
    getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

import Control.Concurrent
import Control.Monad.Logger
import Smos.Web.Server.Handler.Import
import Text.Julius

getCheckoutR :: Handler Html
getCheckoutR = do
  withLogin' $ \un t -> do
    mMonetisation <- runClientOrErr clientGetMonetisation
    status <- runClientOrErr $ clientGetUserSubscription t
    renderUrl <- getUrlRender
    InitiatedCheckoutSession {..} <-
      runClientOrErr $
        clientPostInitiateStripeCheckoutSession
          t
          InitiateStripeCheckoutSession
            { initiateStripeCheckoutSessionSuccessUrl = renderUrl CheckoutSuccessR,
              initiateStripeCheckoutSessionCanceledUrl = renderUrl CheckoutCanceledR
            }
    let stripeForm Monetisation {..} =
          let clientReferenceId = usernameText un
           in $(widgetFile "stripe-form")
    now <- liftIO getCurrentTime
    withNavBar $(widgetFile "checkout")

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = do
  -- Wait a while to allow time for us to deal with the stripe events.
  liftIO $ threadDelay 3_000_000
  redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = do
  -- Wait a while to allow time for us to deal with the stripe events.
  liftIO $ threadDelay 3_000_000
  redirect AccountR
