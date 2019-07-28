{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Single
  ( smosSingle
  , module Smos.Single.Config
  ) where

import Data.Time
import Path
import Path.IO
import System.Exit

import Smos.Single.Config
import Smos.Single.OptParse
import Smos.Single.OptParse.Types

smosSingle :: SmosSingleConfig -> IO ()
smosSingle = runReaderT $ liftIO getSettings >>= archive

archive :: Settings -> Q ()
archive s = liftIO $ print s
