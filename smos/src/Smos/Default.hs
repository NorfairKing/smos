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
                  , exactString "ha" entrySelectHeader
                  -- Movements
                  , exactKey KUp moveUpInEntryForest
                  , exactChar 'k' moveUpInEntryForest
                  , exactKey KDown moveDownInEntryForest
                  , exactChar 'j' moveDownInEntryForest
                  , exactKey KHome moveToFirstEntryForest
                  , exactString "gg" moveToFirstEntryForest
                  , exactKey KEnd moveToLastEntryForest
                  , exactChar 'G' moveToLastEntryForest
                  -- Forest manipulation
                  , exactString "ek" insertEntryAbove
                  , combo
                        [(KeyPress (KChar 'e') []), (KeyPress KUp [])]
                        insertEntryAbove
                  , exactString "ej" insertEntryBelow
                  , combo
                        [(KeyPress (KChar 'e') []), (KeyPress KDown [])]
                        insertEntryBelow
                  , exactChar 'd' deleteCurrentTree
                  -- Extras
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
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
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapContentsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapTimestampsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapPropertiesMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapStateHistoryMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapTagsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        , keyMapLogbookMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyPress (KeyPress (KChar '?') [MMeta]) toggleDebug
                  , exactKeyPress (KeyPress KEnter [MMeta]) toggleDebug
                  ]
        }
