module Smos.Config
    ( SmosConfig(..)
    , module Smos.Actions
    , Map
    , listMatchers
    , exactChar
    , exactKey
    , exactKeyWithMods
    , anyChar
    , KeyMap(..)
    , KeyMapping(..)
    , KeyMappings
    , Action
    , action
    , ActionUsing(..)
    , stop
    , module Graphics.Vty.Input.Events
    ) where

import Data.Map (Map)

import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Smos.Actions
import Smos.Types

listMatchers :: [KeyMapping] -> KeyMappings
listMatchers = id

exactChar :: Char -> Action -> KeyMapping
exactChar c a = exactKey (KChar c) a

exactKey :: Key -> Action -> KeyMapping
exactKey k a = exactKeyWithMods k [] a

exactKeyWithMods :: Key -> [Modifier] -> Action -> KeyMapping
exactKeyWithMods = MapVtyExactly

anyChar :: ActionUsing Char -> KeyMapping
anyChar = MapAnyTypeableChar
