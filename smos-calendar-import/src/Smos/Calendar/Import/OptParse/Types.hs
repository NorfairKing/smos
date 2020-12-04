{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.OptParse.Types where

import Data.Aeson hiding ((<?>))
import Data.List.NonEmpty (NonEmpty (..))
import Network.URI (URI)
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagDebug :: Maybe Bool
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confCalendarImportConfiguration :: !(Maybe CalendarImportConfiguration)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envDebug :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> Report.directoryConfigurationObjectParser
        <*> optionalField "calendar" "Calendar configuration"

data CalendarImportConfiguration = CalendarImportConfiguration
  { calendarImportConfSources :: !(Maybe (NonEmpty SourceConfiguration)),
    calendarImportConfDebug :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance FromJSON CalendarImportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema CalendarImportConfiguration where
  yamlSchema =
    objectParser "CalendarImportConfiguration" $
      CalendarImportConfiguration
        <$> optionalField "sources" "The sources to import from"
        <*> optionalField "debug" "Show the internal structure of every event in its entry's contents."

data SourceConfiguration = SourceConfiguration
  { sourceConfName :: !(Maybe String),
    sourceConfOrigin :: !String,
    sourceConfDestinationFile :: !FilePath
  }
  deriving (Show, Eq)

instance YamlSchema SourceConfiguration where
  yamlSchema =
    objectParser "SourceConfiguration" $
      SourceConfiguration
        <$> optionalField "name" "The name of the source"
        <*> ( requiredField "source" "the url to fetch or file to import"
                <??> [ "If you are using Google, you want to get the URL that has these labels:",
                       "\"Use this address to access this calendar from other applications without making it public.\"",
                       "\"Warning: Only share this address with those you trust to see all event details for this calendar.\""
                     ]
            )
        <*> requiredField "destination" "The destination path within the workflow directory"

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setSources :: !(NonEmpty Source),
    setDebug :: Bool
  }
  deriving (Show, Eq)

data Source = Source
  { sourceName :: Maybe String,
    sourceDestinationFile :: !(Path Rel File),
    sourceOrigin :: !Origin
  }
  deriving (Show, Eq)

data Origin = WebOrigin URI | FileOrigin (Path Abs File)
  deriving (Show, Eq)
