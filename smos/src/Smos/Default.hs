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
                  ]
        , keyMapEntryMatchers =
              listMatchers
                  [ exactChar 'q' stop
                  , exact KEsc stop
                  , exactChar 'h' insertEntryAbove
                  , exactChar 'H' insertEntryBelow
                  ]
        }
