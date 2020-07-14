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
import Data.Maybe
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time
import Rainbow
import Smos.Data
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Entry
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Streaming
import Smos.Report.Time
import Smos.Report.Work
import System.Exit

smosQueryWork :: WorkSettings -> Q ()
smosQueryWork WorkSettings {..} = do
  now <- liftIO getZonedTime
  src <- asks smosQueryConfigReportConfig
  liftIO $ when (not workSetSmartMode && isNothing workSetTime) $ die "No time filter provided."
  wr <-
    produceWorkReport
      src
      workSetHideArchive
      workSetContext
      workSetTimeProperty
      workSetTime
      workSetFilter
      workSetSorter
      workSetChecks
  liftIO $ putTableLn $ renderWorkReport now workSetProjection wr

produceWorkReport ::
  SmosReportConfig ->
  HideArchive ->
  ContextName ->
  PropertyName ->
  Maybe Time ->
  Maybe EntryFilterRel ->
  Maybe Sorter ->
  Set EntryFilterRel ->
  Q WorkReport
produceWorkReport src ha cn pn mtf mf ms checks = do
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
                workReportContextTimeProperty = pn,
                workReportContextTime = mtf,
                workReportContextAdditionalFilter = mf,
                workReportContextContexts = contexts,
                workReportContextChecks = checks
              }
      let dc = smosReportConfigDirectoryConfig src
      wd <- liftIO $ resolveDirWorkflowDir dc
      fmap (finishWorkReport now pn ms)
        $ runConduit
        $ streamSmosFilesFromWorkflowRel ha dc .| parseSmosFilesRel wd
          .| printShouldPrint PrintWarning
          .| smosFileCursors
          .| C.map (uncurry $ makeIntermediateWorkReport wrc)
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
            [agendaTable]
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
    agendaTable = formatAsTable $ map (formatAgendaEntry now) workReportAgendaEntries
