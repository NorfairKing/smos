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
produceWorkReport src cn mf = do
  let contexts = smosReportConfigContexts src
  case M.lookup cn contexts of
    Nothing -> liftIO $ die $ unwords ["Context not found:", T.unpack $ contextNameText cn]
    Just cf -> do
      let f =
            case mf of
              Nothing -> cf
              Just af -> FilterAnd cf af
      let wrc =
            WorkReportContext
              {workReportContextCurrentContext = f, workReportContextContexts = contexts}
      runConduit $
        streamSmosFiles .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
        C.filter (isNextAction . forestCursorCurrent . snd) .|
        C.map (uncurry $ makeWorkReport wrc) .|
        accumulateMonoid

renderWorkReport :: WorkReport -> Table
renderWorkReport WorkReport {..} =
  mconcat $ concat
    [[ formatAsTable $ map (uncurry entryLine) workReportResultEntries]
    , if null workReportEntriesWithoutContext then [] else [ formatAsTable $
      [fore red $ chunk "WARNING, the following Entries don't match any context:"] :
      map (uncurry entryLine) workReportEntriesWithoutContext
    ]]
  where
    entryLine rp e =
      [ rootedPathChunk rp
      , maybe (chunk "") todoStateChunk $ entryState e
      , headerChunk $ entryHeader e
      ]
