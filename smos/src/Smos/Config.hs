module Smos.Config
    ( SmosConfig(..)
    , module Smos.Actions
    , Map
    , listMatchers
    , exact
    , exactChar
    , KeyMap(..)
    , KeyMatch(..)
    , Action(..)
    , stop
    , module Graphics.Vty.Input.Events
    ) where

import qualified Data.Map as M
import Data.Map (Map)

import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Smos.Actions
import Smos.Types

listMatchers :: [(KeyMatch, Action)] -> Map KeyMatch Action
listMatchers = M.fromList

exact :: Key -> Action -> (KeyMatch, Action)
exact k a = (MatchExactly k [], a)

exactChar :: Char -> Action -> (KeyMatch, Action)
exactChar c a = exact (KChar c) a
