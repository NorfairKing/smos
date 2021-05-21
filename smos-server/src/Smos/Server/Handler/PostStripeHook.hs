{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostStripeHook (servePostStripeHook) where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Smos.Server.Handler.Import

servePostStripeHook :: JSON.Value -> ServerHandler NoContent
servePostStripeHook value = do
  logDebugNS "stripe-hook" $
    T.unlines
      [ "Got a request from Stripe:",
        TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
      ]
  undefined
