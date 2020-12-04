module Smos.Convert.Org.OptParse.Types where

import Path

data Flags = Flags
  { flagFromFile :: FilePath,
    flagToFile :: Maybe FilePath
  }
  deriving (Show, Eq)

data Configuration
  = Configuration
  deriving (Show, Eq)

data Settings = Settings
  { setFromFile :: Path Abs File,
    setToFile :: Maybe (Path Abs File)
  }
  deriving (Show, Eq)
