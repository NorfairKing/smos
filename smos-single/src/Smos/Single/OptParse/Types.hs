module Smos.Single.OptParse.Types where

import Autodocodec
import Path
import Smos.Data
import Smos.Directory.Config
import Smos.Directory.OptParse.Types

data Flags = Flags
  { flagTaskPieces :: ![String],
    flagTaskFile :: !(Maybe FilePath),
    flagDirectoryFlags :: !DirectoryFlags
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectorySettingsuration :: !DirectorySettingsuration
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec = dimapCodec Configuration confDirectorySettingsuration codec

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment
  }
  deriving (Show, Eq)

data Settings = Settings
  { setTask :: !Header,
    setTaskFile :: !(Maybe (Path Rel File)),
    setDirectorySettings :: !DirectorySettings
  }
  deriving (Show, Eq)
