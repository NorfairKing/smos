{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Work
  ( work
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import System.Exit

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Filter
import Smos.Report.Next
import Smos.Report.Streaming
import Smos.Report.Work

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

work :: WorkSettings -> Q ()
work WorkSettings {..} = do
  src <- asks smosQueryConfigReportConfig
  wr <- produceWorkReport src workSetContext workSetFilter
  liftIO $ putTableLn $ renderWorkReport wr

produceWorkReport :: SmosReportConfig -> ContextName -> Maybe Filter -> Q WorkReport
produceWorkReport src cn mf =
  case M.lookup cn $ smosReportConfigContexts src of
    Nothing -> liftIO $ die $ unwords ["Context not found:", T.unpack $ contextNameText cn]
    Just cf -> do
      let f =
            case mf of
              Nothing -> cf
              Just af -> FilterAnd cf af
      es <-
        sourceToList $
        streamSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
        C.filter (\(rp, fc) -> filterPredicate f rp fc) .|
        smosCursorCurrents .|
        C.filter (isNextAction . snd)
      pure $ WorkReport {workReportEntries = es}

renderWorkReport :: WorkReport -> Table
renderWorkReport WorkReport {..} =
  formatAsTable $
  flip map workReportEntries $ \(rp, e) ->
    [ rootedPathChunk rp
    , maybe (chunk "") todoStateChunk $ entryState e
    , headerChunk $ entryHeader e
    ]
