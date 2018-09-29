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
                  , exactChar 'k' forestMoveUp
                  , exactKey KUp forestMoveUp
                  , exactChar 'j' forestMoveDown
                  , exactKey KDown forestMoveDown
                  , exactString "gg" forestMoveToFirst
                  , exactChar 'G' forestMoveToLast
                  -- Swaps
                  , modifiedChar 'k' [MMeta] forestSwapUp
                  , modifiedChar 'j' [MMeta] forestSwapDown
                  , modifiedChar 'h' [MMeta] forestPromoteEntry
                  , modifiedChar 'H' [MMeta] forestPromoteSubTree
                  , modifiedChar 'l' [MMeta] forestDemoteEntry
                  , modifiedChar 'L' [MMeta] forestDemoteSubTree
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
                  -- Fast tag manipulation
                  , exactString "gw" $ tagsToggle "work"
                  , exactString "go" $ tagsToggle "online"
                  -- Clocking
                  , exactString "ci" forestClockOutEverywhereAndClockInHere
                  , exactString "co" forestClockOutEverywhere
                  -- Collapsing
                  , exactChar '\t' forestToggleCollapse
                  , exactKey KBackTab forestToggleHideEntireEntry
                  -- Entering contents
                  , combo
                        [KeyPress KEnter [], KeyPress KEnter []]
                        entrySelectContents
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
        , keyMapContentsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , anyChar contentsInsert
                  , exactKey KEnter contentsInsertNewline
                  , exactKey KBS contentsRemove
                  , exactKey KDel contentsDelete
                  , exactKey KLeft contentsMoveLeft
                  , exactKey KRight contentsMoveRight
                  , exactKey KUp contentsMoveUp
                  , exactKey KDown contentsMoveDown
                  , exactKey KHome contentsMoveToStartOfLine
                  , exactKey KEnd contentsMoveToEndOfLine
                  ]
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
