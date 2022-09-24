{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.OptParse.Types where

import Autodocodec
import Data.List.NonEmpty (NonEmpty (..))
import Network.URI (URI)
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagDebug :: Maybe Bool
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envDebug :: !(Maybe Bool)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confCalendarImportConfiguration :: !(Maybe CalendarImportConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull "calendar" "Calendar configuration" .= confCalendarImportConfiguration

data CalendarImportConfiguration = CalendarImportConfiguration
  { calendarImportConfSources :: !(Maybe (NonEmpty SourceConfiguration)),
    calendarImportConfDebug :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance HasCodec CalendarImportConfiguration where
  codec =
    object "CalendarImportConfiguration" $
      CalendarImportConfiguration
        <$> optionalFieldOrNull "sources" "The sources to import from" .= calendarImportConfSources
        <*> optionalFieldOrNull "debug" "Show the internal structure of every event in its entry's contents." .= calendarImportConfDebug

data SourceConfiguration = SourceConfiguration
  { sourceConfName :: !(Maybe String),
    sourceConfOrigin :: !String,
    sourceConfDestinationFile :: !FilePath
  }
  deriving (Show, Eq)

instance HasCodec SourceConfiguration where
  codec =
    object "SourceConfiguration" $
      SourceConfiguration
        <$> optionalFieldOrNull "name" "The name of the source" .= sourceConfName
        <*> requiredFieldWith
          "source"
          ( codec
              <??> [ "If you are using Google, you want to get the URL that has these labels:",
                     "\"Use this address to access this calendar from other applications without making it public.\"",
                     "\"Warning: Only share this address with those you trust to see all event details for this calendar.\""
                   ]
          )
          "the url to fetch or file to import"
          .= sourceConfOrigin
        <*> requiredField "destination" "The destination path within the workflow directory" .= sourceConfDestinationFile

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
