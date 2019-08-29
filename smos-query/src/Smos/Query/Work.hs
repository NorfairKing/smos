{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Work
  ( work
  ) where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time

import System.Exit

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Report.Filter
import Smos.Report.Projection
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
  wr <- produceWorkReport src workSetHideArchive workSetContext workSetFilter workSetChecks
  liftIO $ putTableLn $ renderWorkReport now workSetProjection wr

produceWorkReport ::
     SmosReportConfig -> HideArchive -> ContextName -> Maybe Filter -> Set Filter -> Q WorkReport
produceWorkReport src ha cn mf checks = do
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
              , workReportContextChecks = checks
              }
      runConduit $
        streamSmosFiles ha .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors .|
        C.map (uncurry $ makeWorkReport wrc) .|
        accumulateMonoid

renderWorkReport :: ZonedTime -> NonEmpty Projection -> WorkReport -> Table
renderWorkReport now ne WorkReport {..} =
  mconcat $
  (concat . concat) $
  intersperse [spacer] $
  filter (not . null) $
  [ unlessNull
      workReportAgendaEntries
      [ sectionHeading "Today's agenda:"
      , [formatAsTable $ map (formatAgendaEntry now) (sortAgendaEntries workReportAgendaEntries)]
      ]
  , unlessNull
      workReportResultEntries
      [sectionHeading "Next actions:", [entryTable workReportResultEntries]]
  , unlessNull
      workReportEntriesWithoutContext
      [ warningHeading "WARNING, the following Entries don't match any context:"
      , [entryTable workReportEntriesWithoutContext]
      ]
  , unlessNull
      workReportCheckViolations
      [ warningHeading "WARNING, the following Entries did not pass the checks:"
      , concat $
        flip concatMap (M.toList workReportCheckViolations) $ \(f, violations) ->
          unlessNull violations [warningHeading (renderFilter f), [entryTable violations]]
      ]
  ]
  where
    unlessNull l r =
      if null l
        then []
        else r
    sectionHeading t = heading $ fore white $ chunk t
    warningHeading t = heading $ fore red $ chunk t
    heading c = [formatAsTable $ [[c]]]
    spacer = [formatAsTable $ [[chunk " "]]]
    entryTable = renderEntryTable ne
