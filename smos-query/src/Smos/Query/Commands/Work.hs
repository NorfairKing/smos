{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Work
  ( smosQueryWork,
  )
where

import Conduit
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Report.Entry
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Work
import System.Exit

smosQueryWork :: WorkSettings -> Q ()
smosQueryWork WorkSettings {..} = do
  src <- asks smosQueryConfigReportConfig
  liftIO $ when (not workSetSmartMode && isNothing workSetTime) $ die "No time filter provided."
  now <- liftIO getZonedTime
  let wc = smosReportConfigWorkConfig src
  let contexts = workReportConfigContexts wc
  let baseFilter = workReportConfigBaseFilter wc
  mcf <- forM workSetContext $ \cn ->
    case M.lookup cn contexts of
      Nothing -> liftIO $ die $ unwords ["Context not found:", T.unpack $ contextNameText cn]
      Just cf -> pure cf
  let wrc =
        WorkReportContext
          { workReportContextNow = now,
            workReportContextBaseFilter = baseFilter,
            workReportContextCurrentContext = mcf,
            workReportContextTimeProperty = workSetTimeProperty,
            workReportContextTime = workSetTime,
            workReportContextAdditionalFilter = workSetFilter,
            workReportContextContexts = contexts,
            workReportContextChecks = workSetChecks,
            workReportContextSorter = workSetSorter
          }
  wr <- produceWorkReport workSetHideArchive (smosReportConfigDirectoryConfig src) wrc
  liftIO $ putTableLn $ renderWorkReport now workSetProjection wr

renderWorkReport :: ZonedTime -> NonEmpty Projection -> WorkReport -> Table
renderWorkReport now ne WorkReport {..} =
  mconcat
    $ (concat . concat)
    $ intersperse [spacer]
    $ filter
      (not . null)
      [ unlessNull
          workReportNextBegin
          [ sectionHeading "Next meeting:",
            [formatAsTable $ maybe [] ((: []) . formatAgendaEntry now) workReportNextBegin]
          ],
        unlessNull
          workReportAgendaEntries
          [ sectionHeading "Deadlines:",
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
