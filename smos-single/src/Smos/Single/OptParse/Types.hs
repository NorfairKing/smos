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
  { confDirectoryConfiguration :: !DirectoryConfiguration
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec = dimapCodec Configuration confDirectoryConfiguration codec

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment
  }
  deriving (Show, Eq)

data Settings = Settings
  { setTask :: !Header,
    setTaskFile :: !(Maybe (Path Rel File)),
    setDirectorySettings :: !DirectoryConfig
  }
  deriving (Show, Eq)
