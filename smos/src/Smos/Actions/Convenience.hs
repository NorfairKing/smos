{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Convenience where

import Control.Monad
import Data.Time
import Lens.Micro.Extras

import Smos.Data

import Smos.Types

import Smos.Cursor.Entry
import Smos.Cursor.Header

import Smos.Actions.Utils

convDoneAndWaitForResponse :: Action
convDoneAndWaitForResponse =
    Action
        { actionName = "convDoneAndWaitForResponse"
        , actionFunc =
              do modifyMTodoStateM $ const $ Just "DONE"
                 modifyFileCursor smosFileCursorInsertEntryAfterAndSelectHeader
                 insertHeaderString "for a response"
                 modifyMTodoStateM $ const $ Just "WAITING"
                 modifyEntryCursor entryCursorSelectWhole
        , actionDescription =
              "Mark the current task as 'done' and add a new WAITING entry below"
        }

convRepinged :: Action
convRepinged =
    Action
        { actionName = "convRepinged"
        , actionFunc =
              do me <-
                     gets
                         (fmap
                              (rebuildEntryCursor .
                               view (smosFileCursorSelectedEntryL)) .
                          view editorCursorSmosFileCursorL . smosStateCursor)
                 case me of
                     Nothing -> pure ()
                     Just e -> do
                         now <- liftIO getCurrentTime
                         let e' =
                                 e
                                     { entryStateHistory =
                                           StateHistory
                                               [ StateHistoryEntry
                                                     { stateHistoryEntryNewState =
                                                           entryState e
                                                     , stateHistoryEntryTimestamp =
                                                           now
                                                     }
                                               ]
                                     }
                         modifyMTodoStateM $ const $ Just "DONE"
                         modifyFileCursor
                             smosFileCursorInsertEntryAfterAndSelectHeader
                         insertHeaderString "Ping again"
                         modifyMTodoStateM $ const $ Just "DONE"
                         modifyFileCursor
                             smosFileCursorInsertEntryAfterAndSelectHeader
                         modifyEntryCursor $ const $ makeEntryCursor e'
                         modifyEntryCursor entryCursorSelectWhole
        , actionDescription =
              "Mark the current task as 'done', add a new entry called 'Ping again' and add a new WAITING entry below that, that duplicates the original entry."
        }

insertHeaderString :: String -> SmosM ()
insertHeaderString s =
    modifyHeaderCursorWhenSelectedM $ \hc ->
        foldM (flip headerCursorInsert) hc s
