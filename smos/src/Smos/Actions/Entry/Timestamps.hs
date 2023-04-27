{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry.Timestamps
  ( allTimestampsPlainActions,
    allTimestampsUsingCharActions,
    timestampsSelect,
    timestampsInsert,
    timestampsAppend,
    timestampsMoveLeft,
    timestampsMoveRight,
    timestampsRemove,
    timestampsDelete,
    timestampsToggle,
  )
where

import Data.Maybe
import Smos.Actions.Utils
import Smos.Data.Types
import Smos.Types

allTimestampsPlainActions :: [Action]
allTimestampsPlainActions =
  concat
    [ do
        act <- [timestampsSelect]
        arg <-
          mapMaybe
            timestampName
            [ "AFTER",
              "BEGIN",
              "DEADLINE",
              "END",
              "SCHEDULED"
            ]
        pure $ act arg,
      [ timestampsMoveLeft,
        timestampsMoveRight,
        timestampsRemove,
        timestampsDelete,
        timestampsToggle
      ]
    ]

allTimestampsUsingCharActions :: [ActionUsing Char]
allTimestampsUsingCharActions = [timestampsInsert, timestampsAppend]

timestampsSelect :: TimestampName -> Action
timestampsSelect tsn =
  Action
    { actionName = "timestampsSelect_" <> ActionName t,
      actionFunc = do
        modifyMTimestampsCursorSM $ \mtsc -> do
          lt <- liftIO getLocalTime
          pure $
            Just $
              case mtsc of
                Nothing -> startTimestampsCursor tsn lt
                Just tsc -> timestampsCursorSelectOrAdd tsn lt tsc
        modifyEntryCursor entryCursorSelectTimestamps
        unrecordFileCursorHistory,
      actionDescription = "Select a timestamp for name " <> t
    }
  where
    t = timestampNameText tsn

timestampsInsert :: ActionUsing Char
timestampsInsert =
  ActionUsing
    { actionUsingName = "timestampsInsert",
      actionUsingFunc = \c -> do
        modifyTimestampsCursorM $ timestampsCursorInsertChar c
        unrecordFileCursorHistory,
      actionUsingDescription =
        "Insert a character into the current timestamp cursor, whether that be the name or the timestamp itself"
    }

timestampsAppend :: ActionUsing Char
timestampsAppend =
  ActionUsing
    { actionUsingName = "timestampsAppend",
      actionUsingFunc = \c -> do
        modifyTimestampsCursorM $ timestampsCursorAppendChar c
        unrecordFileCursorHistory,
      actionUsingDescription =
        "Append a character into the current timestamp cursor, whether that be the name or the timestamp itself"
    }

timestampsMoveLeft :: Action
timestampsMoveLeft =
  Action
    { actionName = "timestampsMoveLeft",
      actionFunc = modifyTimestampsCursorM timestampsCursorSelectPrevChar,
      actionDescription = "Move one character to the left in the current timestamps cursor"
    }

timestampsMoveRight :: Action
timestampsMoveRight =
  Action
    { actionName = "timestampsMoveRight",
      actionFunc = modifyTimestampsCursorM timestampsCursorSelectNextChar,
      actionDescription = "Move one character to the right in the current timestamps cursor"
    }

timestampsRemove :: Action
timestampsRemove =
  Action
    { actionName = "timestampsRemove",
      actionFunc = do
        modifyTimestampsCursorMD timestampsCursorRemoveChar
        unrecordFileCursorHistory,
      actionDescription = "Remove one character in the current timestamps cursor"
    }

timestampsDelete :: Action
timestampsDelete =
  Action
    { actionName = "timestampsDelete",
      actionFunc = do
        modifyTimestampsCursorMD timestampsCursorDeleteChar
        unrecordFileCursorHistory,
      actionDescription = "Delete one character  in the current timestamps cursor"
    }

timestampsToggle :: Action
timestampsToggle =
  Action
    { actionName = "timestampsToggle",
      actionFunc = modifyTimestampsCursor timestampsCursorToggleSelected,
      actionDescription = "Switch between selecting the timestamp name or date"
    }
