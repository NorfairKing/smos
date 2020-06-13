{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Work
  ( smosQueryWork,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time
import Rainbow
import Smos.Data
import Smos.Query.Commands.Agenda
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Agenda
import Smos.Report.Entry
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Streaming
import Smos.Report.Work
import System.Exit

smosQueryWork :: WorkSettings -> Q ()
smosQueryWork WorkSettings {..} = do
  now <- liftIO getZonedTime
  src <- asks smosQueryConfigReportConfig
  wr <-
    produceWorkReport
      src
      workSetHideArchive
      workSetContext
      workSetTimeFilter
      workSetFilter
      workSetSorter
      workSetChecks
  liftIO $ putTableLn $ renderWorkReport now workSetProjection wr

produceWorkReport ::
  SmosReportConfig ->
  HideArchive ->
  ContextName ->
  Maybe (Filter Entry) ->
  Maybe EntryFilter ->
  Maybe Sorter ->
  Set EntryFilter ->
  Q WorkReport
produceWorkReport src ha cn mtf mf ms checks = do
  let wc = smosReportConfigWorkConfig src
  let contexts = workReportConfigContexts wc
  let baseFilter = workReportConfigBaseFilter wc
  case M.lookup cn contexts of
    Nothing -> liftIO $ die $ unwords ["Context not found:", T.unpack $ contextNameText cn]
    Just cf -> do
      now <- liftIO getZonedTime
      let wrc =
            WorkReportContext
              { workReportContextNow = now,
                workReportContextBaseFilter = baseFilter,
                workReportContextCurrentContext = cf,
                workReportContextTimeFilter = mtf,
                workReportContextAdditionalFilter = mf,
                workReportContextContexts = contexts,
                workReportContextChecks = checks
              }
      fmap (finishWorkReport ms)
        $ runConduit
        $ streamSmosFiles ha .| parseSmosFiles .| printShouldPrint PrintWarning .| smosFileCursors
          .| C.map (uncurry $ makeWorkReport wrc)
          .| accumulateMonoid

renderWorkReport :: ZonedTime -> NonEmpty Projection -> WorkReport -> Table
renderWorkReport now ne WorkReport {..} =
  mconcat
    $ (concat . concat)
    $ intersperse [spacer]
    $ filter
      (not . null)
      [ unlessNull
          workReportAgendaEntries
          [ sectionHeading "Today's agenda:",
            [formatAsTable $ renderAgendaReportLines now $ insertNowLine now $ map EntryLine $ sortAgendaEntries workReportAgendaEntries]
          ],
        unlessNull
          workReportResultEntries
          [sectionHeading "Next actions:", [entryTable workReportResultEntries]],
        unlessNull
          workReportEntriesWithoutContext
          [ warningHeading "WARNING, the following Entries don't match any context:",
            [entryTable workReportEntriesWithoutContext]
          ],
        unlessNull
          workReportCheckViolations
          [ warningHeading "WARNING, the following Entries did not pass the checks:",
            concat
              $ flip concatMap (M.toList workReportCheckViolations)
              $ \(f, violations) ->
                unlessNull violations [warningHeading (renderFilter f), [entryTable violations]]
          ]
      ]
  where
    unlessNull l r =
      if null l
        then []
        else r
    sectionHeading t = heading $ underline $ fore white $ chunk t
    warningHeading t = heading $ underline $ fore red $ chunk t
    heading c = [formatAsTable [[c]]]
    spacer = [formatAsTable [[chunk " "]]]
    entryTable = renderEntryReport . makeEntryReport ne
