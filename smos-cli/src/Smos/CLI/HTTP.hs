{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Smos.CLI.HTTP
  ( httpLbsWithRetry,
    httpRetryPolicy,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types as HTTP
import Text.Show.Pretty
import UnliftIO

-- | Try three times:
--
--   *  after 1 second
--   *  after 2 seconds
--   *  after 4 seconds
httpRetryPolicy :: RetryPolicy
httpRetryPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

httpLbsWithRetry ::
  (MonadLogger m, MonadUnliftIO m) =>
  HTTP.Request ->
  HTTP.Manager ->
  m (Either HttpException (HTTP.Response LB.ByteString))
httpLbsWithRetry request man = retrying httpRetryPolicy (\_ -> shouldRetryHttpRequest) $ \retryStatus ->
  tryHttpOnce retryStatus request man
    `catches` [ Handler $ \e ->
                  pure
                    ( Left
                        ( HttpExceptionRequest
                            request
                            (InternalException (SomeException (e :: IOError)))
                        )
                    ),
                Handler $ \e -> pure (Left (toHttpException request e)),
                Handler $ \e -> pure (Left (e :: HttpException))
              ]

tryHttpOnce ::
  (MonadLogger m, MonadIO m) =>
  RetryStatus ->
  HTTP.Request ->
  HTTP.Manager ->
  m (Either HttpException (HTTP.Response LB.ByteString))
tryHttpOnce retryStatus request man = do
  when (rsIterNumber retryStatus > 0) $
    logWarnN $
      T.pack $
        unwords
          [ "Retrying request",
            show (getUri request),
            "iteration",
            show $ rsIterNumber retryStatus
          ]
  fmap Right $ do
    response <- liftIO $ httpLbs request man
    let c = HTTP.statusCode (responseStatus response)
    when (c >= 400) $ logWarnN $ T.pack $ unwords [show c, "status code while fetching", show (getUri request)]
    pure response

shouldRetryHttpRequest :: (MonadLogger m) => Either HttpException (Response LB.ByteString) -> m Bool
shouldRetryHttpRequest = \case
  Left exception -> shouldRetryHttpException exception
  Right response -> pure $ shouldRetryStatusCode $ responseStatus response

shouldRetryStatusCode :: Status -> Bool
shouldRetryStatusCode status =
  let c = HTTP.statusCode status
   in c >= 500 && c < 600

shouldRetryHttpException :: (MonadLogger m) => HttpException -> m Bool
shouldRetryHttpException exception = case exception of
  InvalidUrlException _ _ -> pure False
  HttpExceptionRequest request_ exceptionContent -> do
    logWarnN $
      T.pack $
        unlines
          [ unwords ["Something went wrong while fetching", show (getUri request_)],
            ppShow exception
          ]
    pure $ case exceptionContent of
      ResponseTimeout -> True
      ConnectionTimeout -> True
      ConnectionFailure _ -> True
      InternalException _ -> True
      ProxyConnectException _ _ _ -> True
      NoResponseDataReceived -> True
      ResponseBodyTooShort _ _ -> True
      InvalidChunkHeaders -> True
      IncompleteHeaders -> True
      HttpZlibException _ -> True
      ConnectionClosed -> True
      _ -> False
