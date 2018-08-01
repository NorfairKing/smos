module Smos.Config
    ( SmosConfig(..),
    Map
    , listMatchers
    , KeyMap(..)
    , KeyMatch(..)
    , Action(..)
    , stop
    , module Graphics.Vty.Input.Events
    ) where

import qualified Data.Map as M
import Data.Map (Map)

import Graphics.Vty.Input.Events (Key(..))

import Smos.Types

listMatchers :: [(KeyMatch, Action)] -> Map KeyMatch Action
listMatchers = M.fromList
