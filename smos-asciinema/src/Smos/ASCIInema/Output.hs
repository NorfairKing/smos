{-# LANGUAGE OverloadedStrings #-}

module Smos.ASCIInema.Output where

import Conduit
import Control.Concurrent.STM
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import qualified Data.Conduit.Combinators as C
import Data.Time
import GHC.IO.Handle

data OutputView
  = NoOutputView
  | ProgressOutputView
  | DisplayOutputView

outputConduit :: MonadIO m => TVar [(UTCTime, ByteString)] -> Handle -> ConduitT () void m ()
outputConduit outVar h =
  sourceHandle h
    --  .| outputDebugConduit
    .| outputTimerConduit
    .| outputSink outVar

outputSink :: MonadIO m => TVar [(UTCTime, ByteString)] -> ConduitT (UTCTime, ByteString) void m ()
outputSink outVar = awaitForever $ \t -> do
  liftIO $ atomically $ modifyTVar' outVar (t :)

outputTimerConduit :: MonadIO m => ConduitT i (UTCTime, i) m ()
outputTimerConduit = C.mapM $ \i -> (,) <$> liftIO getCurrentTime <*> pure i

outputDebugConduit :: MonadIO m => ConduitT ByteString ByteString m ()
outputDebugConduit = C.mapM $ \bs -> do
  liftIO $ putStrLn $ "Got output: " <> show bs
  pure bs

outputDisplayConduit :: MonadIO m => ConduitT ByteString ByteString m ()
outputDisplayConduit = C.mapM $ \bs -> do
  liftIO $ SB.putStr bs
  pure bs
