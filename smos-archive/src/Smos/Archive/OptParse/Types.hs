module Smos.Archive.OptParse.Types where

import Path

data Flags =
  Flags
    { flagFile :: FilePath
    , flagWorkflowDir :: Maybe FilePath
    , flagArchiveDir :: Maybe FilePath
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Settings =
  Settings
    { setFile :: Path Abs File
    , setWorkflowDir :: Maybe (Path Abs Dir)
    , setArchiveDir :: Maybe (Path Abs Dir)
    }
  deriving (Show, Eq)
