{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Smos

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig = SmosConfig {configKeyMap = defaultKeyMap}

defaultKeyMap :: KeyMap
defaultKeyMap =
    KeyMap
        { keyMapEmptyMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exactKey KEsc stop
                  , exactChar 'h' startHeaderFromEmptyAndSelectHeader
                  , exactChar 'H' startHeaderFromEmptyAndSelectHeader
                  ]
        , keyMapEntryMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exactKey KEsc stop
                  -- Selections
                  , exactChar 'a' entrySelectHeaderAtEnd
                  , exactChar 'A' entrySelectHeaderAtEnd
                  , exactChar 'i' entrySelectHeaderAtStart
                  , exactChar 'I' entrySelectHeaderAtStart
                  -- Movements
                  , exactKey KUp forestMoveUp
                  , exactChar 'k' forestMoveUp
                  , exactKey KDown forestMoveDown
                  , exactChar 'j' forestMoveDown
                  -- Swaps
                  , exactKeyPress (KeyPress KUp [MShift]) forestSwapUp
                  , exactKeyPress (KeyPress KDown [MShift]) forestSwapDown
                  -- Forest manipulation
                  , exactChar 'h' forestInsertEntryAfterAndSelectHeader
                  , exactChar 'H' forestInsertEntryBelowAndSelectHeader
                  -- Deletion
                  , exactChar 'd' forestDeleteCurrentEntry
                  , exactChar 'D' forestDeleteCurrentSubTree
                  -- Fast todo state manipulation
                  , exactString "tt" $ entrySetTodoState "TODO"
                  , exactString "tn" $ entrySetTodoState "NEXT"
                  , exactString "ts" $ entrySetTodoState "STARTED"
                  , exactString "tr" $ entrySetTodoState "READY"
                  , exactString "tw" $ entrySetTodoState "WAITING"
                  , exactString "td" $ entrySetTodoState "DONE"
                  , exactString "tc" $ entrySetTodoState "CANCELLED"
                  , exactString "t " entryUnsetTodoState
                  ]
        , keyMapHeaderMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKey KEnter entrySelectWhole
                  , anyChar headerInsert
                  , exactKey KBS headerRemove
                  , exactKey KDel headerDelete
                  , exactKey KLeft headerMoveLeft
                  , exactKey KRight headerMoveRight
                  , exactKey KHome headerMoveToStart
                  , exactKey KEnd headerMoveToEnd
                  ]
        , keyMapContentsMatchers = listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapTimestampsMatchers =
              listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapPropertiesMatchers =
              listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapStateHistoryMatchers =
              listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapTagsMatchers = listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapLogbookMatchers = listMatchers [exactKey KEsc entrySelectWhole]
        , keyMapHelpMatchers = listMatchers [catchAll selectEditor]
        , keyMapAnyMatchers =
              listMatchers
                  [ exactChar 'u' undo
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        }
