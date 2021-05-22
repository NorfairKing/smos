{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Checkout
  ( getCheckoutR,
    getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

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
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
