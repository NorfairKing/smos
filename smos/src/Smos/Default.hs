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
                  , exactChar 'h' insertEntryAbove
                  , exactChar 'H' insertEntryBelow
                  , exactChar 'd' deleteCurrentTree
                  , exact KUp moveUpInEntryForest
                  , exactChar 'k' moveUpInEntryForest
                  , exact KDown moveDownInEntryForest
                  , exactChar 'j' moveDownInEntryForest
                  , exact KHome moveToFirstEntryForest
                  , exactChar 'g' moveToFirstEntryForest
                  , exact KEnd moveToLastEntryForest
                  , exactChar 'G' moveToLastEntryForest
                  , exactChar '?' toggleHelp
                  , (MatchExactly KEnter [MMeta], toggleDebug)
                  ]
        , keyMapHeaderMatchers = mempty
        , keyMapContentsMatchers = mempty
        , keyMapTimestampsMatchers = mempty
        , keyMapPropertiesMatchers = mempty
        , keyMapStateHistoryMatchers = mempty
        , keyMapTagsMatchers = mempty
        , keyMapLogbookMatchers = mempty
        }
