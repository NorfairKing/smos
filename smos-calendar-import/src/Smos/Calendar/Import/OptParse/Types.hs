{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.OptParse.Types where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Flags
  = Flags
      { flagDirectoryFlags :: !Report.DirectoryFlags,
        flagDestinationFile :: !(Maybe FilePath),
        flagSources :: !(Maybe (NonEmpty String)),
        flagDebug :: Maybe Bool
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
        confCalendarImportConfiguration :: !(Maybe CalendarImportConfiguration)
      }
  deriving (Show, Eq)

data Environment
  = Environment
      { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
        envDestinationFile :: !(Maybe FilePath),
        envSource :: !(Maybe String),
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

data CalendarImportConfiguration
  = CalendarImportConfiguration
      { calendarImportConfDestinationFile :: !(Maybe FilePath),
        calendarImportConfSources :: !(Maybe (NonEmpty String)),
        calendarImportConfDebug :: !(Maybe Bool)
      }
  deriving (Show, Eq)

instance FromJSON CalendarImportConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema CalendarImportConfiguration where
  yamlSchema =
    objectParser "CalendarImportConfiguration" $
      CalendarImportConfiguration
        <$> optionalField "destination" "The destination path within the workflow directory"
        <*> ( optionalField "sources" "The list of urls to fetch and import"
                <??> [ "If you are using Google, you want to get the URL that has these labels:",
                       "\"Use this address to access this calendar from other applications without making it public.\"",
                       "\"Warning: Only share this address with those you trust to see all event details for this calendar.\""
                     ]
            )
        <*> optionalField "debug" "Show the internal structure of every event in its entry's contents."

data Settings
  = Settings
      { setDirectorySettings :: !Report.DirectoryConfig,
        setDestinationFile :: !(Path Rel File),
        setSources :: !(NonEmpty String),
        setDebug :: Bool
      }
  deriving (Show, Eq)
