{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Network.URI (URI)
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import Text.Read

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagLogLevel :: !(Maybe LogLevel),
    flagDebug :: Maybe Bool
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envLogLevel :: !(Maybe LogLevel),
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
  { calendarImportConfSources :: ![SourceConfiguration],
    calendarImportConfLogLevel :: !(Maybe LogLevel),
    calendarImportConfDebug :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance HasCodec CalendarImportConfiguration where
  codec =
    object "CalendarImportConfiguration" $
      CalendarImportConfiguration
        <$> optionalFieldOrNullWithOmittedDefault "sources" [] "The sources to import from" .= calendarImportConfSources
        <*> optionalFieldOrNullWith
          "log-level"
          (bimapCodec parseLogLevel renderLogLevel codec)
          "Minimal severity of error messages"
          .= calendarImportConfLogLevel
        <*> optionalFieldOrNull "debug" "Show the internal structure of every event in its entry's contents." .= calendarImportConfDebug

data SourceConfiguration = SourceConfiguration
  { sourceConfName :: !(Maybe String),
    sourceConfOrigin :: !(Maybe String),
    sourceConfOriginFile :: !(Maybe FilePath),
    sourceConfDestinationFile :: !FilePath
  }
  deriving (Show, Eq)

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
  { setDirectorySettings :: !Report.DirectoryConfig,
    setLogLevel :: !LogLevel,
    setSources :: ![Source],
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

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
