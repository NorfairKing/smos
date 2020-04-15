{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Convenience
  ( allConveniencePlainActions,
    convDoneAndWaitForResponse,
    convRepinged,
    convNewEntryAndClockIn,
  )
where

import Control.Monad
import Data.Time
import Lens.Micro.Extras
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
      actionFunc = do
        me <-
          gets
            ( fmap (rebuildEntryCursor . view smosFileCursorSelectedEntryL)
                . view editorCursorSmosFileCursorL
                . smosStateCursor
            )
        case me of
          Nothing -> pure ()
          Just e -> do
            modifyMTodoStateM $ const $ Just "DONE"
            modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
            insertHeaderString "Ping again"
            modifyMTodoStateM $ const $ Just "DONE"
            modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
            now <- liftIO getCurrentTime
            let e' =
                  e
                    { entryStateHistory =
                        StateHistory
                          [ StateHistoryEntry
                              { stateHistoryEntryNewState = entryState e,
                                stateHistoryEntryTimestamp = now
                              }
                          ]
                    }
            modifyEntryCursor $ const $ makeEntryCursor e'
            modifyEntryCursor entryCursorSelectWhole,
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
