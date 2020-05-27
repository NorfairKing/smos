{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Convenience
  ( allConveniencePlainActions,
    convDoneAndWaitForResponse,
    convRepinged,
    convRespondedButStillWaiting,
    convNewEntryAndClockIn,
  )
where

import Control.Category ((>>>))
import Data.Maybe
import Data.Time
import Lens.Micro
import Smos.Actions.Entry
import Smos.Actions.Forest
import Smos.Actions.Utils
import Smos.Data
import Smos.Types

allConveniencePlainActions :: [Action]
allConveniencePlainActions = [convDoneAndWaitForResponse, convRepinged, convRespondedButStillWaiting, convNewEntryAndClockIn]

convDoneAndWaitForResponse :: Action
convDoneAndWaitForResponse =
  Action
    { actionName = "convDoneAndWaitForResponse",
      actionFunc = modifyFileCursorS $ \sfc -> do
        now <- liftIO getCurrentTime
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            f3 = smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "for a response from " hc)
            f4 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "WAITING"
            f5 = smosFileCursorSelectedEntryL %~ entryCursorSelectHeaderAtEnd
        pure $ (f1 >>> f2 >>> f3 >>> f4 >>> f5) sfc,
      actionDescription =
        "Mark the current task as 'Done', add a new entry called 'Waiting for a response from ' WAITINg entry with the header selected at the end."
    }

convRepinged :: Action
convRepinged =
  Action
    { actionName = "convRepinged",
      actionFunc = modifyFileCursorS $ \sfc -> do
        let e = rebuildEntryCursor $ sfc ^. smosFileCursorSelectedEntryL
        now <- liftIO getCurrentTime
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            f3 = smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Ping again" hc)
            f4 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f5 = smosFileCursorInsertEntryAfterAndSelectHeader
            e' =
              e
                { entryStateHistory =
                    StateHistory
                      [ StateHistoryEntry
                          { stateHistoryEntryNewState = entryState e,
                            stateHistoryEntryTimestamp = now
                          }
                      ]
                }
            f6 = smosFileCursorSelectedEntryL .~ makeEntryCursor e'
            f7 = smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
        pure $ (f1 >>> f2 >>> f3 >>> f4 >>> f5 >>> f6 >>> f7) sfc,
      actionDescription =
        "Mark the current task as 'done', add a new entry called 'Ping again' and add a new WAITING entry below that, that duplicates the original entry."
    }

convRespondedButStillWaiting :: Action
convRespondedButStillWaiting =
  Action
    { actionName = "convRespondedButStillWaiting",
      actionFunc = modifyFileCursorS $ \sfc -> do
        let e = rebuildEntryCursor $ sfc ^. smosFileCursorSelectedEntryL
        now <- liftIO getCurrentTime
        let f1 = smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE"
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            e' =
              e
                { entryStateHistory =
                    StateHistory
                      [ StateHistoryEntry
                          { stateHistoryEntryNewState = entryState e,
                            stateHistoryEntryTimestamp = now
                          }
                      ]
                }
            f3 = smosFileCursorSelectedEntryL .~ makeEntryCursor e'
            f4 = smosFileCursorSelectedEntryL %~ entryCursorSelectWhole
        pure $ (f1 >>> f2 >>> f3 >>> f4) sfc,
      actionDescription =
        "Mark the current task as 'done' and add a new entry below that duplicates the original entry."
    }

convNewEntryAndClockIn :: Action
convNewEntryAndClockIn =
  Action
    { actionName = "convNewEntryAndClockIn",
      actionFunc = do
        actionFunc forestInsertEntryAfterAndSelectHeader
        actionFunc entrySelectWhole
        actionFunc forestClockOutEverywhereInAllFilesAndClockInHere
        actionFunc entrySelectHeaderAtEnd,
      actionDescription = "Create a new entry and clock in immediately"
    }
