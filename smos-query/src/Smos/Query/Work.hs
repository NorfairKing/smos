{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Work
  ( work
  ) where

import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.Text (Text)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Filter
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

work :: WorkSettings -> Q ()
work WorkSettings {..} = do
  es <-
    sourceToList $
    streamSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
    C.filter (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) workSetFilter) .|
    smosCursorCurrents .|
    C.map snd
  liftIO $ print es
