{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Convenience
  ( allConveniencePlainActions,
    convDoneAndWaitForResponse,
    convRepinged,
    convNewEntryAndClockIn,
  )
where

import Control.Category ((>>>))
import Control.Monad
import Data.Maybe
import Data.Time
import Lens.Micro
import Smos.Actions.Entry
import Smos.Actions.Forest
import Smos.Actions.Utils
import Smos.Data
import Smos.Types

allConveniencePlainActions :: [Action]
allConveniencePlainActions = [convDoneAndWaitForResponse, convRepinged, convNewEntryAndClockIn]

convDoneAndWaitForResponse :: Action
convDoneAndWaitForResponse =
  Action
    { actionName = "convDoneAndWaitForResponse",
      actionFunc = do
        modifyMTodoStateM $ const $ Just "DONE"
        modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
        insertHeaderString "for a response from "
        modifyMTodoStateM $ const $ Just "WAITING"
        modifyEntryCursor entryCursorSelectHeaderAtEnd,
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
        let f1 = (smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE")
            f2 = smosFileCursorInsertEntryAfterAndSelectHeader
            f3 = smosFileCursorSelectedEntryL . entryCursorHeaderCursorL %~ (\hc -> fromMaybe hc $ headerCursorAppendString "Ping again" hc)
            f4 = (smosFileCursorSelectedEntryL . entryCursorStateHistoryCursorL %~ stateHistoryCursorSetTodoState now "DONE")
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

insertHeaderString :: String -> SmosM ()
insertHeaderString s = modifyHeaderCursorWhenSelectedM $ \hc -> foldM (flip headerCursorInsert) hc s

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
