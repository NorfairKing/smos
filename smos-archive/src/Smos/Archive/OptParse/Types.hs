module Smos.Archive.OptParse.Types where

import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Flags
  = Flags
      { flagFile :: FilePath,
        flagDirectoryFlags :: !Report.DirectoryFlags
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDirectoryConfiguration :: !Report.DirectoryConfiguration
      }
  deriving (Show, Eq)

instance YamlSchema Configuration where
  yamlSchema = Configuration <$> yamlSchema

data Environment
  = Environment
      { envDirectoryEnvironment :: !Report.DirectoryEnvironment
      }
  deriving (Show, Eq)

data Settings
  = Settings
      { setFile :: !(Path Abs File),
        setDirectorySettings :: !Report.DirectoryConfig
      }
  deriving (Show, Eq)
