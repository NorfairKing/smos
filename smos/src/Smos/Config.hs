module Smos.Config
    ( SmosConfig(..)
    , module Smos.Actions
    , Map
    , listMatchers
    , exactString
    , combo
    , exactChar
    , exactKey
    , exactKeyPress
    , KeyPress(..)
    , anyChar
    , catchAll
    , KeyMap(..)
    , KeyMapping(..)
    , KeyMappings
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

exactString :: [Char] -> Action -> KeyMapping
exactString cs a =
    case cs of
        [] -> error "Cannot have an empty key combination"
        [c] -> exactChar c a
        (c:cs_) -> MapCombination (KeyPress (KChar c) []) $ exactString cs_ a

combo :: [KeyPress] -> Action -> KeyMapping
combo kps a =
    case kps of
        [] -> error "Cannot have an empty key combination"
        [kp] -> exactKeyPress kp a
        (kp:kps_) -> MapCombination kp $ combo kps_ a

exactChar :: Char -> Action -> KeyMapping
exactChar c a = exactKey (KChar c) a

exactKey :: Key -> Action -> KeyMapping
exactKey k a = exactKeyPress (KeyPress k []) a

exactKeyPress :: KeyPress -> Action -> KeyMapping
exactKeyPress = MapVtyExactly

anyChar :: ActionUsing Char -> KeyMapping
anyChar = MapAnyTypeableChar

catchAll :: Action -> KeyMapping
catchAll = MapCatchAll
