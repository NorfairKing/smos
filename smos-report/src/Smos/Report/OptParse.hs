{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Report.OptParse where

import Autodocodec
import Control.Applicative
import Control.Arrow
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import qualified Env
import GHC.Generics (Generic)
import qualified OptEnvConf
import Options.Applicative
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Report.Agenda.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

combineToSettings ::
  ReportSettings -> Flags -> Environment -> Maybe Configuration -> IO ReportSettings
combineToSettings src Flags {..} Environment {..} mc = do
  reportSettingDirectorySettings <- combineToDirectorySettings (reportSettingDirectorySettings src) flagDirectoryFlags envDirectoryEnvironment (confDirectoryConf <$> mc)
  reportSettingWaitingSettings <- combineToWaitingReportSettings (reportSettingWaitingSettings src) (mc >>= confWaitingReportConf)
  reportSettingStuckSettings <- combineToStuckReportSettings (reportSettingStuckSettings src) (mc >>= confStuckReportConf)
  reportSettingWorkSettings <- combineToWorkReportSettings (reportSettingWorkSettings src) (mc >>= confWorkReportConf)
  reportSettingFreeSettings <- combineToFreeReportSettings (reportSettingFreeSettings src) (mc >>= confFreeReportConf)
  pure $ ReportSettings {..}

combineToWaitingReportSettings :: WaitingReportSettings -> Maybe WaitingReportConfiguration -> IO WaitingReportSettings
combineToWaitingReportSettings wrc mc = do
  let WaitingReportSettings _ = undefined
  pure $
    wrc
      { waitingReportSettingThreshold = fromMaybe defaultWaitingThreshold $ mc >>= waitingReportConfThreshold
      }

combineToStuckReportSettings :: StuckReportSettings -> Maybe StuckReportConfiguration -> IO StuckReportSettings
combineToStuckReportSettings wrc mc = do
  let StuckReportSettings _ = undefined
  pure $
    wrc
      { stuckReportSettingThreshold = fromMaybe defaultStuckThreshold $ mc >>= stuckReportConfThreshold
      }

combineToWorkReportSettings :: WorkReportSettings -> Maybe WorkReportConfiguration -> IO WorkReportSettings
combineToWorkReportSettings wrc mc = do
  let WorkReportSettings _ _ _ _ _ _ = undefined
  pure $
    wrc
      { workReportSettingBaseFilter =
          (mc >>= workReportConfBaseFilter) <|> workReportSettingBaseFilter wrc,
        workReportSettingChecks = fromMaybe (workReportSettingChecks wrc) (mc >>= workReportConfChecks),
        workReportSettingContexts = fromMaybe (workReportSettingContexts wrc) (mc >>= workReportConfContexts),
        workReportSettingTimeProperty = mc >>= workReportConfTimeFilterProperty,
        workReportSettingProjection = fromMaybe defaultProjection (mc >>= workReportConfProjection),
        workReportSettingSorter = mc >>= workReportConfSorter
      }

combineToFreeReportSettings :: FreeReportSettings -> Maybe FreeReportConfiguration -> IO FreeReportSettings
combineToFreeReportSettings wrc mc = do
  let FreeReportSettings _ _ = undefined
  pure $
    wrc
      { freeReportSettingEarliestTimeOfDay = maybe (freeReportSettingEarliestTimeOfDay defaultFreeReportSettings) freeReportConfigurationEarliestTimeOfDay mc,
        freeReportSettingLatestTimeOfDay = maybe (freeReportSettingLatestTimeOfDay defaultFreeReportSettings) freeReportConfigurationLatestTimeOfDay mc
      }

parseFlags :: Parser Flags
parseFlags =
  Flags <$> parseDirectoryFlags

parseHistoricityFlag :: Parser (Maybe AgendaHistoricity)
parseHistoricityFlag =
  optional (flag' HistoricalAgenda (long "historical") <|> flag' FutureAgenda (long "future"))

parseHideArchiveFlag :: Parser (Maybe HideArchive)
parseHideArchiveFlag =
  optional
    ( flag' HideArchive (mconcat [long "hide-archived", help "ignore archived files."])
        <|> flag'
          Don'tHideArchive
          (mconcat [short 'a', long "show-archived", help "Don't ignore archived files."])
    )

parseContextNameArg :: Parser (Maybe ContextName)
parseContextNameArg =
  optional $ argument (ContextName <$> str) (mconcat [metavar "CONTEXT", help "The context that you are in"])

parseTimeFilterArg :: Parser (Maybe Time)
parseTimeFilterArg =
  optional $
    argument
      (eitherReader (parseTime . T.pack))
      (mconcat [metavar "TIME_FILTER", help "A filter to filter by time"])

parseFilterOptionsRel :: Parser (Maybe EntryFilter)
parseFilterOptionsRel =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( option
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
          (mconcat [short 'f', long "filter", metavar "FILTER", help "A filter to filter entries by"])
      )

parseFilterArgsRel :: Parser (Maybe EntryFilter)
parseFilterArgsRel =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to filter entries by"])
      )

parseProjectFilterArgs :: Parser (Maybe ProjectFilter)
parseProjectFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to filter projects by"])
      )

parseProjectionArgs :: Parser (Maybe (NonEmpty Projection))
parseProjectionArgs =
  NE.nonEmpty . catMaybes
    <$> many
      ( option
          (Just <$> eitherReader (parseProjection . T.pack))
          ( mconcat
              [ long "add-column",
                long "project",
                metavar "PROJECTION",
                help "A projection to project entries onto fields"
              ]
          )
      )

parseSorterArgs :: Parser (Maybe Sorter)
parseSorterArgs =
  fmap (foldl1 AndThen) . NE.nonEmpty . catMaybes
    <$> many
      ( option
          (Just <$> eitherReader (parseSorter . T.pack))
          (mconcat [long "sort", metavar "SORTER", help "A sorter to sort entries by"])
      )

parseTimeBlock :: Parser (Maybe TimeBlock)
parseTimeBlock =
  optional
    ( asum
        [ flag' DayBlock $ mconcat [long "day-block", help "blocks of one day"],
          flag' WeekBlock $ mconcat [long "week-block", help "blocks of one week"],
          flag' MonthBlock $ mconcat [long "month-block", help "blocks of one month"],
          flag' YearBlock $ mconcat [long "year-block", help "blocks of one year"],
          flag' OneBlock $ mconcat [long "one-block", help "a single block"]
        ]
    )

parsePeriod :: Parser (Maybe Period)
parsePeriod =
  parseBeginEnd
    <|> optional
      ( asum
          [ flag' Yesterday (mconcat [long "yesterday", help "yesterday"]),
            flag' Today (mconcat [long "today", help "today"]),
            flag' Tomorrow (mconcat [long "tomorrow", help "tomorrow"]),
            flag' LastWeek (mconcat [long "last-week", help "last week"]),
            flag' PastWeek (mconcat [long "past-week", help "the past week"]),
            flag' ThisWeek (mconcat [long "this-week", help "this week"]),
            flag' ComingWeek (mconcat [long "coming-week", help "the coming week"]),
            flag' NextWeek (mconcat [long "next-week", help "next week"]),
            flag' LastMonth (mconcat [long "last-month", help "last month"]),
            flag' PastMonth (mconcat [long "past-month", help "the past month"]),
            flag' ThisMonth (mconcat [long "this-month", help "this month"]),
            flag' ComingMonth (mconcat [long "coming-month", help "the coming month"]),
            flag' NextMonth (mconcat [long "next-month", help "next month"]),
            flag' LastYear (mconcat [long "last-year", help "last year"]),
            flag' PastYear (mconcat [long "past-year", help "the past year"]),
            flag' ThisYear (mconcat [long "this-year", help "this year"]),
            flag' ComingYear (mconcat [long "coming-year", help "the coming year"]),
            flag' NextYear (mconcat [long "next-year", help "next year"]),
            flag' AllTime (mconcat [long "all-time", help "all time"])
          ]
      )
  where
    parseBeginEnd :: Parser (Maybe Period)
    parseBeginEnd =
      ( \mb me ->
          case (mb, me) of
            (Nothing, Nothing) -> Nothing
            (Just begin, Nothing) -> Just (BeginOnly begin)
            (Nothing, Just end) -> Just (EndOnly end)
            (Just begin, Just end) -> Just (BeginEnd begin end)
      )
        <$> optional
          ( option
              (maybeReader parseBegin)
              (mconcat [long "begin", metavar "DAY", help "start date (inclusive)"])
          )
        <*> optional
          ( option
              (maybeReader parseEnd)
              (mconcat [long "end", metavar "DAY", help "end time (inclusive)"])
          )
    parseBegin :: String -> Maybe Day
    parseBegin s = parseLocalDay s
    parseEnd :: String -> Maybe Day
    parseEnd s = addDays 1 <$> parseLocalDay s
    parseLocalDay :: String -> Maybe Day
    parseLocalDay = parseTimeM True defaultTimeLocale "%F"

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> directoryEnvironmentParser

data Flags = Flags
  { flagDirectoryFlags :: DirectoryFlags
  }

data Environment = Environment
  { envDirectoryEnvironment :: DirectoryEnvironment
  }

data Configuration = Configuration
  { confDirectoryConf :: !DirectoryConfiguration,
    confWaitingReportConf :: !(Maybe WaitingReportConfiguration),
    confStuckReportConf :: !(Maybe StuckReportConfiguration),
    confWorkReportConf :: !(Maybe WorkReportConfiguration),
    confFreeReportConf :: !(Maybe FreeReportConfiguration)
  }
  deriving stock (Show, Generic)

instance Validity Configuration

instance HasObjectCodec Configuration where
  objectCodec =
    Configuration
      <$> objectCodec
        .= confDirectoryConf
      <*> optionalFieldOrNull "waiting" "The waiting report configuration"
        .= confWaitingReportConf
      <*> optionalFieldOrNull "stuck" "The stuck projects report configuration"
        .= confStuckReportConf
      <*> optionalFieldOrNull "work" "The work report configuration"
        .= confWorkReportConf
      <*> optionalFieldOrNull "free" "The free report configuration"
        .= confFreeReportConf

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confDirectoryConf = defaultDirectoryConfiguration,
      confWaitingReportConf = Nothing,
      confStuckReportConf = Nothing,
      confWorkReportConf = Nothing,
      confFreeReportConf = Nothing
    }

data WaitingReportConfiguration = WaitingReportConfiguration
  { waitingReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Generic)

instance Validity WaitingReportConfiguration

instance HasCodec WaitingReportConfiguration where
  codec =
    object "WaitingReportConfiguration" $
      WaitingReportConfiguration
        <$> optionalFieldOrNull "threshold" "waiting report threshold to consider waiting entries 'overdue'"
          .= waitingReportConfThreshold

data StuckReportConfiguration = StuckReportConfiguration
  { stuckReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Generic)

instance Validity StuckReportConfiguration

instance HasCodec StuckReportConfiguration where
  codec =
    object "StuckReportConfiguration" $
      StuckReportConfiguration
        <$> optionalFieldOrNull "threshold" "stuck report threshold to consider stuck projects 'overdue'"
          .= stuckReportConfThreshold

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

instance Validity ContextName

data WorkReportConfiguration = WorkReportConfiguration
  { workReportConfBaseFilter :: !(Maybe EntryFilter),
    workReportConfChecks :: !(Maybe (Set EntryFilter)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving stock (Show, Generic)

instance Validity WorkReportConfiguration

instance HasCodec WorkReportConfiguration where
  codec =
    object "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalFieldOrNull "base-filter" "The base work filter"
          .= workReportConfBaseFilter
        <*> optionalFieldOrNull "checks" "Checks for the work report"
          .= workReportConfChecks
        <*> optionalFieldOrNull "contexts" "Contexts for the work report"
          .= workReportConfContexts
        <*> optionalFieldOrNull "time-filter" "The property to use to filter by time"
          .= workReportConfTimeFilterProperty
        <*> optionalFieldOrNull "columns" "The columns in the report"
          .= workReportConfProjection
        <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows"
          .= workReportConfSorter

defaultWorkReportConfiguration :: WorkReportConfiguration
defaultWorkReportConfiguration =
  WorkReportConfiguration
    { workReportConfBaseFilter = Nothing,
      workReportConfChecks = Nothing,
      workReportConfContexts = Nothing,
      workReportConfTimeFilterProperty = Nothing,
      workReportConfProjection = Nothing,
      workReportConfSorter = Nothing
    }

data FreeReportConfiguration = FreeReportConfiguration
  { freeReportConfigurationEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeReportConfigurationLatestTimeOfDay :: !(Maybe TimeOfDay)
  }
  deriving (Show, Generic)

instance Validity FreeReportConfiguration

instance HasCodec FreeReportConfiguration where
  codec = object "Configuration" objectCodec

instance HasObjectCodec FreeReportConfiguration where
  objectCodec =
    FreeReportConfiguration
      <$> optionalField "earliest" "the earliest time of day to consider free"
        .= freeReportConfigurationEarliestTimeOfDay
      <*> optionalField "latest" "the latest time of day to consider free"
        .= freeReportConfigurationLatestTimeOfDay

data ReportSettings = ReportSettings
  { reportSettingDirectorySettings :: !DirectorySettings,
    reportSettingWaitingSettings :: !WaitingReportSettings,
    reportSettingStuckSettings :: !StuckReportSettings,
    reportSettingWorkSettings :: !WorkReportSettings,
    reportSettingFreeSettings :: !FreeReportSettings
  }

instance OptEnvConf.HasParser ReportSettings where
  settingsParser = parseReportSettings

{-# ANN parseReportSettings ("NOCOVER" :: String) #-}
parseReportSettings :: OptEnvConf.Parser ReportSettings
parseReportSettings = do
  reportSettingDirectorySettings <- OptEnvConf.settingsParser
  reportSettingWaitingSettings <- OptEnvConf.subSettings "waiting"
  reportSettingStuckSettings <- OptEnvConf.subSettings "stuck"
  reportSettingWorkSettings <- OptEnvConf.subSettings "work"
  reportSettingFreeSettings <- OptEnvConf.subSettings "free"
  pure ReportSettings {..}

defaultReportSettings :: ReportSettings
defaultReportSettings =
  ReportSettings
    { reportSettingDirectorySettings = defaultDirectorySettings,
      reportSettingWaitingSettings = defaultWaitingReportSettings,
      reportSettingStuckSettings = defaultStuckReportSettings,
      reportSettingWorkSettings = defaultWorkReportSettings,
      reportSettingFreeSettings = defaultFreeReportSettings
    }

data WaitingReportSettings = WaitingReportSettings
  { waitingReportSettingThreshold :: Time
  }

instance OptEnvConf.HasParser WaitingReportSettings where
  settingsParser = parseWaitingReportSettings

{-# ANN parseWaitingReportSettings ("NOCOVER" :: String) #-}
parseWaitingReportSettings :: OptEnvConf.Parser WaitingReportSettings
parseWaitingReportSettings = do
  waitingReportSettingThreshold <- OptEnvConf.setting []
  pure WaitingReportSettings {..}

defaultWaitingReportSettings :: WaitingReportSettings
defaultWaitingReportSettings =
  WaitingReportSettings
    { waitingReportSettingThreshold = defaultWaitingThreshold
    }

defaultWaitingThreshold :: Time
defaultWaitingThreshold = Days 7

data StuckReportSettings = StuckReportSettings
  { stuckReportSettingThreshold :: Time
  }

instance OptEnvConf.HasParser StuckReportSettings where
  settingsParser = parseStuckReportSettings

{-# ANN parseStuckReportSettings ("NOCOVER" :: String) #-}
parseStuckReportSettings :: OptEnvConf.Parser StuckReportSettings
parseStuckReportSettings = do
  stuckReportSettingThreshold <- OptEnvConf.setting []
  pure StuckReportSettings {..}

defaultStuckReportSettings :: StuckReportSettings
defaultStuckReportSettings =
  StuckReportSettings
    { stuckReportSettingThreshold = defaultStuckThreshold
    }

defaultStuckThreshold :: Time
defaultStuckThreshold = Weeks 3

data WorkReportSettings = WorkReportSettings
  { workReportSettingBaseFilter :: Maybe EntryFilter,
    workReportSettingChecks :: Set EntryFilter,
    workReportSettingContexts :: Map ContextName EntryFilter,
    workReportSettingTimeProperty :: Maybe PropertyName,
    workReportSettingProjection :: NonEmpty Projection,
    workReportSettingSorter :: Maybe Sorter
  }

instance OptEnvConf.HasParser WorkReportSettings where
  settingsParser = parseWorkReportSettings

{-# ANN parseWorkReportSettings ("NOCOVER" :: String) #-}
parseWorkReportSettings :: OptEnvConf.Parser WorkReportSettings
parseWorkReportSettings = do
  stuckReportSettingThreshold <- OptEnvConf.setting []
  pure WorkReportSettings {..}

defaultWorkReportSettings :: WorkReportSettings
defaultWorkReportSettings =
  WorkReportSettings
    { workReportSettingBaseFilter = Just defaultWorkBaseFilter,
      workReportSettingChecks = S.empty,
      workReportSettingContexts = M.empty,
      workReportSettingTimeProperty = Nothing,
      workReportSettingProjection = defaultProjection,
      workReportSettingSorter = Nothing
    }

defaultProjection :: NonEmpty Projection
defaultProjection = OntoFile :| [OntoState, OntoHeader]

defaultWorkBaseFilter :: EntryFilter
defaultWorkBaseFilter =
  FilterSnd $
    FilterWithinCursor $
      FilterEntryTodoState $
        FilterMaybe False $
          FilterOr (FilterSub "NEXT") (FilterSub "STARTED")

data FreeReportSettings = FreeReportSettings
  { freeReportSettingEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeReportSettingLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

instance OptEnvConf.HasParser FreeReportSettings where
  settingsParser = parseFreeReportSettings

{-# ANN parseFreeReportSettings ("NOCOVER" :: String) #-}
parseFreeReportSettings :: OptEnvConf.Parser FreeReportSettings
parseFreeReportSettings = do
  freeReportSettingEarliestTimeOfDay <- OptEnvConf.setting []
  freeReportSettingLatestTimeOfDay <- OptEnvConf.setting []
  pure FreeReportSettings {..}

defaultFreeReportSettings :: FreeReportSettings
defaultFreeReportSettings =
  FreeReportSettings
    { freeReportSettingEarliestTimeOfDay = Just defaultEarliestFreeTimeOfDay,
      freeReportSettingLatestTimeOfDay = Just defaultLatestFreeTimeOfDay
    }

defaultEarliestFreeTimeOfDay :: TimeOfDay
defaultEarliestFreeTimeOfDay = TimeOfDay 08 00 00

defaultLatestFreeTimeOfDay :: TimeOfDay
defaultLatestFreeTimeOfDay = TimeOfDay 22 00 00
