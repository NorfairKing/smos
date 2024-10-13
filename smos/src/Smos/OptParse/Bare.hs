module Smos.OptParse.Bare (getPathArgument) where

import OptEnvConf
import Paths_smos (version)
import Smos.Types

getPathArgument :: IO (Maybe StartingPath)
getPathArgument = runParser version "Smos TUI editor" (optional settingsParser)
