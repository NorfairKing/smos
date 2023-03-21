module Smos.Config
  ( SmosConfig (..),
    module Smos.Actions,
    Map,
    -- Matchers
    listMatchers,
    exactString,
    combo,
    exactChar,
    modifiedChar,
    exactKey,
    exactKeyPress,
    KeyPress (..),
    anyChar,
    catchAll,
    KeyMap (..),
    FileKeyMap (..),
    BrowserKeyMap (..),
    ReportsKeyMap (..),
    NextActionReportKeyMap (..),
    WaitingReportKeyMap (..),
    TimestampsReportKeyMap (..),
    StuckReportKeyMap (..),
    WorkReportKeyMap (..),
    HelpKeyMap (..),
    KeyMapping (..),
    KeyMappings,
    ActionUsing (..),
    quit,
    module Graphics.Vty.Input.Events,
  )
where

import Data.Map (Map)
import Graphics.Vty.Input.Events (Key (..), Modifier (..))
import Smos.Actions
import Smos.Keys
import Smos.Types

listMatchers :: [KeyMapping] -> KeyMappings
listMatchers = id

exactString :: String -> Action -> KeyMapping
exactString cs a =
  case cs of
    [] -> error "Cannot have an empty key combination"
    [c] -> exactChar c a
    (c : cs_) -> MapCombination (KeyPress (KChar c) []) $ exactString cs_ a

combo :: [KeyPress] -> Action -> KeyMapping
combo kps a =
  case kps of
    [] -> error "Cannot have an empty key combination"
    [kp] -> exactKeyPress kp a
    (kp : kps_) -> MapCombination kp $ combo kps_ a

exactChar :: Char -> Action -> KeyMapping
exactChar c = exactKey (KChar c)

modifiedChar :: Char -> [Modifier] -> Action -> KeyMapping
modifiedChar c mods = exactKeyPress (KeyPress (KChar c) mods)

exactKey :: Key -> Action -> KeyMapping
exactKey k = exactKeyPress (KeyPress k [])

exactKeyPress :: KeyPress -> Action -> KeyMapping
exactKeyPress = MapVtyExactly

anyChar :: ActionUsing Char -> KeyMapping
anyChar = MapAnyTypeableChar

catchAll :: Action -> KeyMapping
catchAll = MapCatchAll
