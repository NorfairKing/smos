module Smos.Archive.OptParse.Types where

import Path

data Flags =
  Flags
    { flagFile :: FilePath
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Settings =
  Settings
    { setFile :: Path Abs File
    }
  deriving (Show, Eq)
