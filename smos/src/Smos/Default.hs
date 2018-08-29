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
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
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
                -- Forest manipulation
              , exactChar 'h' forestInsertEntryAfterAndSelectHeader
              , exactChar 'H' forestInsertEntryBelowAndSelectHeader
              , exactChar 'd' forestDeleteCurrentTree
                -- Extras
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
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
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapContentsMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapTimestampsMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapPropertiesMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapStateHistoryMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapTagsMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapLogbookMatchers =
          listMatchers
              [ exactKey KEsc entrySelectWhole
              , exactKeyPress (KeyPress (KChar '?') [MMeta]) selectHelp
              , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
              ]
    , keyMapHelpMatchers = listMatchers [catchAll selectEditor]
    }
