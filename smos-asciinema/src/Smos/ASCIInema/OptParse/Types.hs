{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.ASCIInema.OptParse.Types where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity
import Data.Word
import Data.Yaml
import GHC.Generics (Generic)
import Path
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command = CommandRecord FilePath
  deriving (Show, Eq)

data Flags
  = Flags
      { flagDirectoryFlags :: !Report.DirectoryFlags
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
        confASCIInemaConfiguration :: !(Maybe ASCIInemaConfiguration)
      }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = Configuration <$> yamlSchema <*> objectParser "Configuration" (optionalField "asciinema" "The asciinema configuration")

data ASCIInemaConfiguration
  = ASCIInemaConfiguration
  deriving (Show, Eq)

instance FromJSON ASCIInemaConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaConfiguration where
  yamlSchema =
    objectParser "ASCIInemaConfiguration" $
      pure ASCIInemaConfiguration

data Environment
  = Environment
      { envDirectoryEnvironment :: !Report.DirectoryEnvironment
      }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchRecord (Path Abs File)
  deriving (Show, Eq)

data Settings
  = Settings
      { setDirectorySettings :: !Report.DirectoryConfig
      }
  deriving (Show, Eq)
