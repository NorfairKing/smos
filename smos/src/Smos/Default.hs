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
                  , exactChar 'e' startHeaderFromEmpty
                  , exactChar 'E' startHeaderFromEmpty
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapEntryMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exactKey KEsc stop
                  -- Selections
                  , exactChar 'a' entrySelectHeaderAtStart
                  , exactChar 'i' entrySelectHeaderAtEnd
                  -- Movements
                  , exactKey KUp forestMoveUp
                  , exactChar 'k' forestMoveUp
                  , exactKey KDown forestMoveDown
                  , exactChar 'j' forestMoveDown
                  , exactKey KHome forestMoveToFirstTree
                  , exactString "gg" forestMoveToFirstTree
                  , exactKey KEnd forestMoveToLastTree
                  , exactChar 'G' forestMoveToLastTree
                  -- Forest manipulation
                  , exactString "ek" forestInsertEntryAbove
                  , combo
                        [(KeyPress (KChar 'e') []), (KeyPress KUp [])]
                        forestInsertEntryAbove
                  , exactString "ej" forestInsertEntryBelow
                  , combo
                        [(KeyPress (KChar 'e') []), (KeyPress KDown [])]
                        forestInsertEntryBelow
                  , exactChar 'd' forestDeleteCurrentTree
                  -- Extras
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapHeaderMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , anyChar headerInsert
                  , exactKey KBS headerRemove
                  , exactKey KDel headerDelete
                  , exactKey KLeft headerMoveLeft
                  , exactKey KRight headerMoveRight
                  , exactKey KHome headerMoveToStart
                  , exactKey KEnd headerMoveToEnd
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapContentsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapTimestampsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapPropertiesMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapStateHistoryMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapTagsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapLogbookMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleHelp
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapHelpMatchers = listMatchers [catchAll toggleHelp]
        }
