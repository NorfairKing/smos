module Smos.Single.OptParse.Types where

import Autodocodec
import Path
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report

data Flags = Flags
  { flagTaskPieces :: ![String],
    flagTaskFile :: !(Maybe FilePath),
    flagDirectoryFlags :: !Report.DirectoryFlags
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec = dimapCodec Configuration confDirectoryConfiguration codec

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment
  }
  deriving (Show, Eq)

data Settings = Settings
  { setTask :: !Header,
    setTaskFile :: !(Maybe (Path Rel File)),
    setDirectorySettings :: !Report.DirectoryConfig
  }
  deriving (Show, Eq)
