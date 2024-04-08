{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Network.URI (URI)
import Path
import Smos.CLI.Logging ()
import Smos.Directory.OptParse.Types

data Flags = Flags
  { flagDirectoryFlags :: !DirectoryFlags,
    flagLogLevel :: !(Maybe LogLevel),
    flagDebug :: Maybe Bool
  }

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment,
    envLogLevel :: !(Maybe LogLevel),
    envDebug :: !(Maybe Bool)
  }

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confCalendarImportConfiguration :: !(Maybe CalendarImportConfiguration)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull "calendar" "Calendar configuration" .= confCalendarImportConfiguration

data CalendarImportConfiguration = CalendarImportConfiguration
  { calendarImportConfSources :: ![SourceConfiguration],
    calendarImportConfLogLevel :: !(Maybe LogLevel),
    calendarImportConfDebug :: !(Maybe Bool)
  }

instance HasCodec CalendarImportConfiguration where
  codec =
    object "CalendarImportConfiguration" $
      CalendarImportConfiguration
        <$> optionalFieldOrNullWithOmittedDefault "sources" [] "The sources to import from" .= calendarImportConfSources
        <*> optionalFieldOrNull "log-level" "Minimal severity of error messages" .= calendarImportConfLogLevel
        <*> optionalFieldOrNull "debug" "Show the internal structure of every event in its entry's contents." .= calendarImportConfDebug

data SourceConfiguration = SourceConfiguration
  { sourceConfName :: !(Maybe String),
    sourceConfOrigin :: !(Maybe String),
    sourceConfOriginFile :: !(Maybe FilePath),
    sourceConfDestinationFile :: !FilePath
  }
  deriving (Eq)

instance HasCodec SourceConfiguration where
  codec =
    object "SourceConfiguration" $
      SourceConfiguration
        <$> optionalFieldOrNull "name" "The name of the source" .= sourceConfName
        <*> optionalFieldOrNullWith
          "source"
          ( codec
              <??> [ "If you are using Google, you want to get the URL that has these labels:",
                     "\"Use this address to access this calendar from other applications without making it public.\"",
                     "\"Warning: Only share this address with those you trust to see all event details for this calendar.\"",
                     "For more info, see https://support.google.com/calendar/answer/37648?hl=en#zippy=%2Cget-your-calendar-view-only."
                   ]
          )
          "the url to fetch or file to import"
          .= sourceConfOrigin
        <*> optionalFieldOrNull
          "source-file"
          "the file that contains the url to fetch or file to import"
          .= sourceConfOriginFile
        <*> requiredField "destination" "The destination path within the workflow directory" .= sourceConfDestinationFile

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setLogLevel :: !LogLevel,
    setSources :: ![Source],
    setDebug :: Bool
  }

data Source = Source
  { sourceName :: Maybe String,
    sourceDestinationFile :: !(Path Rel File),
    sourceOrigin :: !Origin
  }

data Origin
  = WebOrigin URI
  | FileOrigin (Path Abs File)
