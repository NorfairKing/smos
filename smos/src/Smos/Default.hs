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
                  , exact KEsc stop
                  , exactChar 'h' startHeaderFromEmpty
                  , exactChar 'H' startHeaderFromEmpty
                  , (MatchExactly KEnter [MMeta], toggleDebug)
                  ]
        , keyMapEntryMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exact KEsc stop
                  -- Selections
                  , exactChar 'a' entrySelectHeader
                  , exactChar 'c' entrySelectContents
                  , exactChar 't' entrySelectTimestamps
                  , exactChar 'p' entrySelectProperties
                  , exactChar 's' entrySelectStateHistory
                  , exactChar 'o' entrySelectTags
                  , exactChar 'l' entrySelectLogbook
                  -- Movements
                  , exact KUp moveUpInEntryForest
                  , exactChar 'k' moveUpInEntryForest
                  , exact KDown moveDownInEntryForest
                  , exactChar 'j' moveDownInEntryForest
                  , exact KHome moveToFirstEntryForest
                  , exactChar 'g' moveToFirstEntryForest
                  , exact KEnd moveToLastEntryForest
                  , exactChar 'G' moveToLastEntryForest
                  -- Forest manipulation
                  , exactChar 'h' insertEntryAbove
                  , exactChar 'H' insertEntryBelow
                  , exactChar 'd' deleteCurrentTree
                  -- Extras
                  , exactChar '?' toggleHelp
                  , (MatchExactly KEnter [MMeta], toggleDebug)
                  ]
        , keyMapHeaderMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapContentsMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapTimestampsMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapPropertiesMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapStateHistoryMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapTagsMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        , keyMapLogbookMatchers =
              listMatchers
                  [exact KEsc entrySelectWhole, exactChar '?' toggleHelp]
        }
