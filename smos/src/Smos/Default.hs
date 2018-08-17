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
                  , exactChar 'h' startHeaderFromEmpty
                  , exactChar 'H' startHeaderFromEmpty
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapEntryMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exactKey KEsc stop
                  -- Selections
                  , exactChar 'a' entrySelectHeader
                  , exactChar 'c' entrySelectContents
                  , exactChar 't' entrySelectTimestamps
                  , exactChar 'p' entrySelectProperties
                  , exactChar 's' entrySelectStateHistory
                  , exactChar 'o' entrySelectTags
                  , exactChar 'l' entrySelectLogbook
                  -- Movements
                  , exactKey KUp moveUpInEntryForest
                  , exactChar 'k' moveUpInEntryForest
                  , exactKey KDown moveDownInEntryForest
                  , exactChar 'j' moveDownInEntryForest
                  , exactKey KHome moveToFirstEntryForest
                  , exactChar 'g' moveToFirstEntryForest
                  , exactKey KEnd moveToLastEntryForest
                  , exactChar 'G' moveToLastEntryForest
                  -- Forest manipulation
                  , exactChar 'h' insertEntryAbove
                  , exactChar 'H' insertEntryBelow
                  , exactChar 'd' deleteCurrentTree
                  -- Extras
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
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
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapContentsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapTimestampsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapPropertiesMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapStateHistoryMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapTagsMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        , keyMapLogbookMatchers =
              listMatchers
                  [ exactKey KEsc entrySelectWhole
                  , exactKeyWithMods (KChar '?') [MMeta] toggleDebug
                  , exactKeyWithMods KEnter [MMeta] toggleDebug
                  ]
        }
