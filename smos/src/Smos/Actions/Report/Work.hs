{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions.Report.Work where

import Cursor.Map
import Cursor.Simple.List.NonEmpty
import Data.Time
import Data.Time.Zones
import Lens.Micro
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Cursor.Report.Entry
import Smos.Directory.Archive
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Report.OptParse.Types
import Smos.Report.Work
import Smos.Types

allPlainReportWorkActions :: [Action]
allPlainReportWorkActions =
  [ reportWork,
    prevWork,
    nextWork,
    firstWork,
    lastWork,
    enterWorkFile,
    selectWorkReport,
    selectWorkFilter,
    removeWorkFilter,
    deleteWorkFilter
  ]

allReportWorkUsingActions :: [ActionUsing Char]
allReportWorkUsingActions =
  [ insertWorkFilter,
    appendWorkFilter
  ]

reportWork :: Action
reportWork =
  Action
    { actionName = "reportWork",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        src <- asks configReportSettings
        let ds = reportSettingDirectorySettings src
        zone <- liftIO loadLocalTZ
        now <- liftIO getCurrentTime
        wd <- liftIO $ resolveDirWorkflowDir ds
        pd <- liftIO $ resolveDirProjectsDir ds
        let mpd = stripProperPrefix wd pd
        let wc = reportSettingWorkSettings src
        let wac = reportSettingWaitingSettings src
        let sc = reportSettingStuckSettings src
        let ctx =
              WorkReportContext
                { workReportContextTimeZone = zone,
                  workReportContextNow = now,
                  workReportContextProjectsSubdir = mpd,
                  workReportContextBaseFilter = workReportSettingBaseFilter wc,
                  workReportContextCurrentContext = Nothing,
                  workReportContextTimeProperty = workReportSettingTimeProperty wc,
                  workReportContextTime = Nothing,
                  workReportContextAdditionalFilter = Nothing,
                  workReportContextContexts = workReportSettingContexts wc,
                  workReportContextChecks = workReportSettingChecks wc,
                  workReportContextSorter = workReportSettingSorter wc,
                  workReportContextWaitingThreshold = waitingReportSettingThreshold wac,
                  workReportContextStuckThreshold = stuckReportSettingThreshold sc
                }

        wrc <- liftIO $ produceWorkReportCursor HideArchive DontPrint ds ctx
        -- If there are no contexts, we don't care about the entries without context
        let wrc' =
              if null (workReportSettingContexts wc)
                then wrc {workReportCursorEntriesWithoutContext = emptyEntryReportCursor}
                else wrc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportWork wrc'
            },
      actionDescription = "Work report"
    }

prevWork :: Action
prevWork =
  Action
    { actionName = "prevWork",
      actionFunc = modifyWorkReportCursorM workReportCursorPrev,
      actionDescription = "Select the previous entry in the work report"
    }

nextWork :: Action
nextWork =
  Action
    { actionName = "nextWork",
      actionFunc = modifyWorkReportCursorM workReportCursorNext,
      actionDescription = "Select the next entry in the work report"
    }

firstWork :: Action
firstWork =
  Action
    { actionName = "firstWork",
      actionFunc = modifyWorkReportCursor workReportCursorFirst,
      actionDescription = "Select the first entry in the work report"
    }

lastWork :: Action
lastWork =
  Action
    { actionName = "lastWork",
      actionFunc = modifyWorkReportCursor workReportCursorLast,
      actionDescription = "Select the last entry in the work report"
    }

enterWorkFile :: Action
enterWorkFile =
  Action
    { actionName = "enterWorkFile",
      actionFunc = do
        ss <- get
        case editorCursorReportCursor $ smosStateCursor ss of
          Just rc -> case rc of
            ReportWork wrc -> do
              dc <- asks $ reportSettingDirectorySettings . configReportSettings
              wd <- liftIO $ resolveDirWorkflowDir dc
              let switchToEntryReportEntryCursor ad EntryReportEntryCursor {..} = switchToCursor (ad </> entryReportEntryCursorFilePath) $ Just $ makeSmosFileCursorFromSimpleForestCursor entryReportEntryCursorForestCursor
                  switchToSelectedInEntryReportCursor ad erc =
                    case entryReportCursorBuildSmosFileCursor ad erc of
                      Nothing -> pure ()
                      Just (afp, sfc) -> switchToCursor afp $ Just sfc
              case workReportCursorSelection wrc of
                NextBeginSelected ->
                  mapM_
                    (switchToEntryReportEntryCursor wd)
                    (workReportCursorNextBeginCursor wrc)
                WithoutContextSelected -> switchToSelectedInEntryReportCursor wd (workReportCursorEntriesWithoutContext wrc)
                CheckViolationsSelected -> case workReportCursorCheckViolations wrc of
                  Nothing -> pure ()
                  Just mc ->
                    let kvc = mc ^. mapCursorElemL
                        erc = foldKeyValueCursor (\_ x -> x) (\_ x -> x) kvc
                     in switchToSelectedInEntryReportCursor wd erc
                DeadlinesSelected -> switchToSelectedInEntryReportCursor wd (timestampsReportCursorEntryReportCursor (workReportCursorDeadlinesCursor wrc))
                OngoingSelected -> switchToSelectedInEntryReportCursor wd (workReportCursorOngoingEntries wrc)
                WaitingSelected -> switchToSelectedInEntryReportCursor wd (waitingReportCursorEntryReportCursor (workReportCursorOverdueWaiting wrc))
                StuckSelected -> case stuckReportCursorSelectedFile (workReportCursorOverdueStuck wrc) of
                  Nothing -> pure ()
                  Just rf -> switchToFile $ wd </> rf
                LimboSelected -> case workReportCursorLimboProjects wrc of
                  Nothing -> pure ()
                  Just nec -> switchToFile $ wd </> nonEmptyCursorCurrent nec
                ResultsSelected -> switchToSelectedInEntryReportCursor wd (workReportCursorResultEntries wrc)
            _ -> pure ()
          Nothing -> pure (),
      actionDescription = "Select the last entry in the work report"
    }

insertWorkFilter :: ActionUsing Char
insertWorkFilter =
  ActionUsing
    { actionUsingName = "insertWorkFilter",
      actionUsingDescription = "Insert a character into the filter bar",
      actionUsingFunc = \a -> modifyWorkReportCursorM $ workReportCursorInsert a
    }

appendWorkFilter :: ActionUsing Char
appendWorkFilter =
  ActionUsing
    { actionUsingName = "appendWorkFilter",
      actionUsingDescription = "Append a character onto the filter bar",
      actionUsingFunc = \a -> modifyWorkReportCursorM $ workReportCursorAppend a
    }

removeWorkFilter :: Action
removeWorkFilter =
  Action
    { actionName = "removeWorkFilter",
      actionDescription = "Remove the character in filter bar before cursor",
      actionFunc = modifyWorkReportCursorM workReportCursorRemove
    }

deleteWorkFilter :: Action
deleteWorkFilter =
  Action
    { actionName = "deleteWorkFilter",
      actionDescription = "Remove the character in filter bar under cursor",
      actionFunc = modifyWorkReportCursorM workReportCursorDelete
    }

selectWorkReport :: Action
selectWorkReport =
  Action
    { actionName = "selectWorkReport",
      actionDescription = "Select the work report",
      actionFunc = modifyWorkReportCursorM workReportCursorSelectReport
    }

selectWorkFilter :: Action
selectWorkFilter =
  Action
    { actionName = "selectWorkFilter",
      actionDescription = "Select the work filter bar",
      actionFunc = modifyWorkReportCursorM workReportCursorSelectFilter
    }
