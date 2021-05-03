{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Work
  ( smosQueryWork,
  )
where

import Conduit
import qualified Data.Map as M
import qualified Data.Text as T
import Smos.Query.Commands.Import
import Smos.Report.Work

smosQueryWork :: WorkSettings -> Q ()
smosQueryWork WorkSettings {..} = do
  now <- liftIO getZonedTime
  mcf <- forM workSetContext $ \cn ->
    case M.lookup cn workSetContexts of
      Nothing -> dieQ $ unwords ["Context not found:", T.unpack $ contextNameText cn]
      Just cf -> pure cf
  dc <- asks envDirectoryConfig
  wd <- liftIO $ resolveDirWorkflowDir dc
  pd <- liftIO $ resolveDirProjectsDir dc
  let mpd = stripProperPrefix wd pd
  let wrc =
        WorkReportContext
          { workReportContextNow = now,
            workReportContextProjectsSubdir = mpd,
            workReportContextBaseFilter = workSetBaseFilter,
            workReportContextCurrentContext = mcf,
            workReportContextTimeProperty = workSetTimeProperty,
            workReportContextTime = workSetTime,
            workReportContextAdditionalFilter = workSetFilter,
            workReportContextContexts = workSetContexts,
            workReportContextChecks = workSetChecks,
            workReportContextSorter = workSetSorter,
            workReportContextWaitingThreshold = workSetWaitingThreshold,
            workReportContextStuckThreshold = workSetStuckThreshold
          }
  sp <- getShouldPrint
  wr <- produceWorkReport workSetHideArchive sp dc wrc
  cc <- asks envColourSettings
  outputChunks $
    renderWorkReport
      cc
      now
      workSetContexts
      workSetWaitingThreshold
      workSetStuckThreshold
      workSetProjection
      wr

renderWorkReport :: ColourSettings -> ZonedTime -> Map ContextName EntryFilterRel -> Word -> Word -> NonEmpty Projection -> WorkReport -> [Chunk]
renderWorkReport cc now ctxs waitingThreshold stuckThreshold ne WorkReport {..} =
  mconcat $
    concat $
      intercalate [spacer] $
        filter
          (not . null)
          [ unlessNull
              workReportNextBegin
              [ sectionHeading "Next meeting",
                [formatAsBicolourTable cc $ maybe [] ((: []) . formatAgendaEntry now) workReportNextBegin]
              ],
            unlessNull
              ctxs
              $ unlessNull
                workReportEntriesWithoutContext
                [ warningHeading
                    "Entries without context",
                  [entryTable workReportEntriesWithoutContext]
                ],
            unlessNull
              workReportCheckViolations
              ( flip concatMap (M.toList workReportCheckViolations) $
                  \(f, violations) ->
                    unlessNull violations [warningHeading ("Check violation for " <> renderFilter f), [entryTable violations]]
              ),
            unlessNull
              workReportAgendaEntries
              [ sectionHeading "Deadlines",
                [agendaTable]
              ],
            unlessNull
              workReportOverdueWaiting
              [ warningHeading "Overdue Waiting Entries",
                [waitingTable]
              ],
            unlessNull
              workReportOverdueStuck
              [ warningHeading "Overdue Stuck Projects",
                [stuckTable]
              ],
            unlessNull
              workReportResultEntries
              [ sectionHeading "Next actions",
                [entryTable workReportResultEntries]
              ]
          ]
  where
    unlessNull l r =
      if null l
        then []
        else r
    sectionHeading t = heading $ underline $ fore white $ chunk t
    warningHeading t = heading $ underline $ fore red $ chunk ("WARNING: " <> t)
    heading c = [formatAsBicolourTable cc [[c]]]
    spacer = [formatAsBicolourTable cc [[chunk " "]]]
    entryTable = renderEntryReport cc . makeEntryReport ne
    agendaTable = formatAsBicolourTable cc $ map (formatAgendaEntry now) workReportAgendaEntries
    waitingTable = formatAsBicolourTable cc $ map (formatWaitingEntry waitingThreshold (zonedTimeToUTC now)) workReportOverdueWaiting
    stuckTable = formatAsBicolourTable cc $ map (formatStuckReportEntry stuckThreshold (zonedTimeToUTC now)) workReportOverdueStuck
