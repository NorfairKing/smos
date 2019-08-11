{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Work
  ( work
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time

import System.Exit

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Filter
import Smos.Report.Streaming
import Smos.Report.Work

import Smos.Query.Agenda
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

work :: WorkSettings -> Q ()
work WorkSettings {..} = do
  now <- liftIO getZonedTime
  src <- asks smosQueryConfigReportConfig
  wr <- produceWorkReport src workSetContext workSetFilter
  liftIO $ putTableLn $ renderWorkReport now wr

produceWorkReport :: SmosReportConfig -> ContextName -> Maybe Filter -> Q WorkReport
produceWorkReport src cn mf = do
  let contexts = smosReportConfigContexts src
  case M.lookup cn contexts of
    Nothing -> liftIO $ die $ unwords ["Context not found:", T.unpack $ contextNameText cn]
    Just cf -> do
      now <- liftIO getZonedTime
      let wrc =
            WorkReportContext
              { workReportContextNow = now
              , workReportContextBaseFilter = smosReportConfigWorkBaseFilter src
              , workReportContextCurrentContext = cf
              , workReportContextAdditionalFilter = mf
              , workReportContextContexts = contexts
              }
      runConduit $
        streamSmosFiles HideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
        C.map (uncurry $ makeWorkReport wrc) .|
        accumulateMonoid

renderWorkReport :: ZonedTime -> WorkReport -> Table
renderWorkReport now WorkReport {..} =
  mconcat $
  concat
    [ sectionHeading  $ "Today's agenda:"
    , [formatAsTable $ map (formatAgendaEntry now) (sortAgendaEntries workReportAgendaEntries)]
    , spacer
    , sectionHeading $ "Next actions:"
    , [formatAsTable $ map (uncurry entryLine) workReportResultEntries]
    , if null workReportEntriesWithoutContext
        then []
        else concat
               [ spacer
               , heading $
                 fore red $ chunk "WARNING, the following Entries don't match any context:"
               , [formatAsTable $ map (uncurry entryLine) workReportEntriesWithoutContext]
               ]
    ]
  where
    sectionHeading t =
       heading $ fore white $ chunk t
    heading c = [formatAsTable $ [[c]]]
    spacer = [formatAsTable $ [[chunk " "]]]
    entryLine rp e =
      [ rootedPathChunk rp
      , maybe (chunk "") todoStateChunk $ entryState e
      , headerChunk $ entryHeader e
      ]
