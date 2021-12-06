module Smos.Archive.OptParse.Types where

import Autodocodec
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report

data Arguments = Arguments !Command !(Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command = CommandArchiveFile !FilePath
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec = dimapCodec Configuration confDirectoryConfiguration codec

data Instructions = Instructions !Dispatch !Settings
  deriving (Show, Eq)

data Dispatch
  = DispatchArchiveFile !(Path Abs File)
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig
  }
  deriving (Show, Eq)
