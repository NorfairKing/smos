{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Convenience where

import Control.Monad

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
                 modifyHeaderCursorWhenSelectedM $ \hc ->
                     foldM
                         (flip headerCursorInsert)
                         hc
                         ("for a response" :: [Char])
                 modifyMTodoStateM $ const $ Just "WAITING"
                 modifyEntryCursor entryCursorSelectWhole
        , actionDescription =
              "Mark the current task as 'done' and add a new WAITING entry below"
        }
