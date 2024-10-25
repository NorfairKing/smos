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
import GHC.Generics (Generic)
import OptEnvConf
import Path
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

-- combineToSettings ::
--   ReportSettings -> Flags -> Environment -> Maybe Configuration -> IO ReportSettings
-- combineToSettings src Flags {..} Environment {..} mc = do
--   reportSettingDirectorySettings <- combineToDirectorySettings (reportSettingDirectorySettings src) flagDirectoryFlags envDirectoryEnvironment (confDirectoryConf <$> mc)
--   reportSettingWaitingSettings <- combineToWaitingReportSettings (reportSettingWaitingSettings src) (mc >>= confWaitingReportConf)
--   reportSettingStuckSettings <- combineToStuckReportSettings (reportSettingStuckSettings src) (mc >>= confStuckReportConf)
--   reportSettingWorkSettings <- combineToWorkReportSettings (reportSettingWorkSettings src) (mc >>= confWorkReportConf)
--   reportSettingFreeSettings <- combineToFreeReportSettings (reportSettingFreeSettings src) (mc >>= confFreeReportConf)
--   pure $ ReportSettings {..}
--
-- combineToWaitingReportSettings :: WaitingReportSettings -> Maybe WaitingReportConfiguration -> IO WaitingReportSettings
-- combineToWaitingReportSettings wrc mc = do
--   let WaitingReportSettings _ = undefined
--   pure $
--     wrc
--       { waitingReportSettingThreshold = fromMaybe defaultWaitingThreshold $ mc >>= waitingReportConfThreshold
--       }
--
-- combineToStuckReportSettings :: StuckReportSettings -> Maybe StuckReportConfiguration -> IO StuckReportSettings
-- combineToStuckReportSettings wrc mc = do
--   let StuckReportSettings _ = undefined
--   pure $
--     wrc
--       { stuckReportSettingThreshold = fromMaybe defaultStuckThreshold $ mc >>= stuckReportConfThreshold
--       }
--
-- combineToWorkReportSettings :: WorkReportSettings -> Maybe WorkReportConfiguration -> IO WorkReportSettings
-- combineToWorkReportSettings wrc mc = do
--   let WorkReportSettings _ _ _ _ _ _ = undefined
--   pure $
--     wrc
--       { workReportSettingBaseFilter =
--           (mc >>= workReportConfBaseFilter) <|> workReportSettingBaseFilter wrc,
--         workReportSettingChecks = fromMaybe (workReportSettingChecks wrc) (mc >>= workReportConfChecks),
--         workReportSettingContexts = fromMaybe (workReportSettingContexts wrc) (mc >>= workReportConfContexts),
--         workReportSettingTimeProperty = mc >>= workReportConfTimeFilterProperty,
--         workReportSettingProjection = fromMaybe defaultProjection (mc >>= workReportConfProjection),
--         workReportSettingSorter = mc >>= workReportConfSorter
--       }
--
-- combineToFreeReportSettings :: FreeReportSettings -> Maybe FreeReportConfiguration -> IO FreeReportSettings
-- combineToFreeReportSettings wrc mc = do
--   let FreeReportSettings _ _ = undefined
--   pure $
--     wrc
--       { freeReportSettingEarliestTimeOfDay = maybe (freeReportSettingEarliestTimeOfDay defaultFreeReportSettings) freeReportConfigurationEarliestTimeOfDay mc,
--         freeReportSettingLatestTimeOfDay = maybe (freeReportSettingLatestTimeOfDay defaultFreeReportSettings) freeReportConfigurationLatestTimeOfDay mc
--       }
--
-- parseFlags :: Parser Flags
-- parseFlags =
--   Flags <$> parseDirectoryFlags

parseFilterOptions :: Parser EntryFilter
parseFilterOptions =
  foldFilterAnd
    <$> someNonEmpty
      ( setting
          [ help "A filter to filter entries by",
            option,
            reader $ eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack),
            short 'f',
            long "filter",
            metavar "FILTER"
          ]
      )

parseFilterArgs :: Parser EntryFilter
parseFilterArgs =
  foldFilterAnd
    <$> someNonEmpty
      ( setting
          [ help "A filter to filter entries by",
            argument,
            reader $ eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack),
            metavar "FILTER"
          ]
      )

parseFileFilterArgs :: Parser (Maybe (Filter (Path Rel File)))
parseFileFilterArgs =
  fmap foldFilterOr . NE.nonEmpty
    <$> many
      ( setting
          [ help "A filter to smos files by",
            argument,
            reader $ eitherReader $ left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack,
            metavar "FILTER"
          ]
      )

parseProjectFilterArgs :: Parser (Maybe ProjectFilter)
parseProjectFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( setting
          [ argument,
            reader $ eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack),
            metavar "FILTER",
            help "A filter to filter projects by"
          ]
      )

parseProjectionArgs :: Parser (Maybe (NonEmpty Projection))
parseProjectionArgs =
  NE.nonEmpty
    <$> many
      ( setting
          [ option,
            reader $ eitherReader (parseProjection . T.pack),
            long "add-column",
            long "project",
            metavar "PROJECTION",
            help "A projection to project entries onto fields"
          ]
      )

parseSorterOptions :: Parser Sorter
parseSorterOptions =
  foldl1 AndThen
    <$> someNonEmpty
      ( setting
          [ option,
            reader $ eitherReader $ parseSorter . T.pack,
            long "sort",
            metavar "SORTER",
            help "A sorter to sort entries by"
          ]
      )

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

instance Validity ContextName

instance HasCodec ContextName where
  codec = dimapCodec ContextName contextNameText codec

instance HasParser ContextName where
  settingsParser =
    setting
      [ help "The context that you are in",
        argument,
        reader $ ContextName <$> str,
        env "CONTEXT",
        conf "context",
        metavar "CONTEXT"
      ]

-- data WorkReportConfiguration = WorkReportConfiguration
--   { workReportConfBaseFilter :: !(Maybe EntryFilter),
--     workReportConfChecks :: !(Maybe (Set EntryFilter)),
--     workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
--     workReportConfTimeFilterProperty :: Maybe PropertyName,
--     workReportConfProjection :: Maybe (NonEmpty Projection),
--     workReportConfSorter :: Maybe Sorter
--   }
--
-- instance HasCodec WorkReportConfiguration where
--   codec =
--     object "WorkReportConfiguration" $
--       WorkReportConfiguration
--         <$> optionalFieldOrNull "base-filter" "The base work filter"
--           .= workReportConfBaseFilter
--         <*> optionalFieldOrNull "checks" "Checks for the work report"
--           .= workReportConfChecks
--         <*> optionalFieldOrNull "contexts" "Contexts for the work report"
--           .= workReportConfContexts
--         <*> optionalFieldOrNull "time-filter" "The property to use to filter by time"
--           .= workReportConfTimeFilterProperty
--         <*> optionalFieldOrNull "columns" "The columns in the report"
--           .= workReportConfProjection
--         <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows"
--           .= workReportConfSorter
--
-- defaultWorkReportConfiguration :: WorkReportConfiguration
-- defaultWorkReportConfiguration =
--   WorkReportConfiguration
--     { workReportConfBaseFilter = Nothing,
--       workReportConfChecks = Nothing,
--       workReportConfContexts = Nothing,
--       workReportConfTimeFilterProperty = Nothing,
--       workReportConfProjection = Nothing,
--       workReportConfSorter = Nothing
--     }
--
-- data FreeReportConfiguration = FreeReportConfiguration
--   { freeReportConfigurationEarliestTimeOfDay :: !(Maybe TimeOfDay),
--     freeReportConfigurationLatestTimeOfDay :: !(Maybe TimeOfDay)
--   }
--
-- instance HasCodec FreeReportConfiguration where
--   codec = object "Configuration" objectCodec
--
-- instance HasObjectCodec FreeReportConfiguration where
--   objectCodec =
--     FreeReportConfiguration
--       <$> optionalField "earliest" "the earliest time of day to consider free"
--         .= freeReportConfigurationEarliestTimeOfDay
--       <*> optionalField "latest" "the latest time of day to consider free"
--         .= freeReportConfigurationLatestTimeOfDay
--
data ReportSettings = ReportSettings
  { reportSettingDirectorySettings :: !DirectorySettings,
    reportSettingWaitingSettings :: !WaitingReportSettings,
    reportSettingStuckSettings :: !StuckReportSettings,
    reportSettingWorkSettings :: !WorkReportSettings,
    reportSettingFreeSettings :: !FreeReportSettings
  }

instance HasParser ReportSettings where
  settingsParser = parseReportSettings

{-# ANN parseReportSettings ("NOCOVER" :: String) #-}
parseReportSettings :: Parser ReportSettings
parseReportSettings = do
  reportSettingDirectorySettings <- settingsParser
  reportSettingWaitingSettings <- subSettings "waiting"
  reportSettingStuckSettings <- subSettings "stuck"
  reportSettingWorkSettings <- subSettings "work"
  reportSettingFreeSettings <- subSettings "free"
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

instance HasParser WaitingReportSettings where
  settingsParser = parseWaitingReportSettings

{-# ANN parseWaitingReportSettings ("NOCOVER" :: String) #-}
parseWaitingReportSettings :: Parser WaitingReportSettings
parseWaitingReportSettings = do
  waitingReportSettingThreshold <-
    setting
      [ help "waiting report threshold to consider waiting entries 'overdue'",
        reader $ eitherReader $ parseTime . T.pack,
        name "threshold",
        metavar "TIME",
        value defaultWaitingThreshold
      ]
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

instance HasParser StuckReportSettings where
  settingsParser = parseStuckReportSettings

{-# ANN parseStuckReportSettings ("NOCOVER" :: String) #-}
parseStuckReportSettings :: Parser StuckReportSettings
parseStuckReportSettings = do
  stuckReportSettingThreshold <-
    setting
      [ help "stuck report threshold to consider stuck projects 'overdue'",
        reader $ eitherReader $ parseTime . T.pack,
        name "threshold",
        metavar "TIME",
        value defaultStuckThreshold
      ]
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

instance HasParser WorkReportSettings where
  settingsParser = parseWorkReportSettings

{-# ANN parseWorkReportSettings ("NOCOVER" :: String) #-}
parseWorkReportSettings :: Parser WorkReportSettings
parseWorkReportSettings = do
  workReportSettingBaseFilter <- parseWorkBaseFilter
  workReportSettingChecks <- parseWorkChecks
  workReportSettingContexts <- parseWorkContexts
  workReportSettingTimeProperty <- parseWorkTimeProperty
  workReportSettingProjection <- parseProjectionOptions
  workReportSettingSorter <-
    optional $
      setting
        [ help "The sorter to use to sort the rows",
          reader $ eitherReader $ parseSorter . T.pack,
          name "sorter",
          metavar "SORTER"
        ]
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

parseWorkBaseFilter :: Parser (Maybe EntryFilter)
parseWorkBaseFilter =
  optional $
    setting
      [ help "The base work filter",
        reader $ eitherReader $ left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack,
        name "base-filter",
        metavar "FILTER",
        value defaultWorkBaseFilter
      ]

parseWorkContexts :: Parser (Map ContextName EntryFilter)
parseWorkContexts =
  setting
    [ help "Contexts for the work report",
      conf "contexts",
      value M.empty
    ]

parseWorkChecks :: Parser (Set EntryFilter)
parseWorkChecks =
  setting
    [ help "Checks for the work report",
      conf "checks",
      value S.empty
    ]

defaultWorkBaseFilter :: EntryFilter
defaultWorkBaseFilter =
  FilterSnd $
    FilterWithinCursor $
      FilterEntryTodoState $
        FilterMaybe False $
          FilterOr (FilterSub "NEXT") (FilterSub "STARTED")

parseProjectionOptions :: Parser (NonEmpty Projection)
parseProjectionOptions =
  setting
    [ help "The columns in the report",
      reader $ commaSeparated $ eitherReader $ parseProjection . T.pack,
      name "columns",
      metavar "COLUMNS",
      value defaultProjection
    ]

defaultProjection :: NonEmpty Projection
defaultProjection = OntoFile :| [OntoState, OntoHeader]

parseWorkTimeProperty :: Parser (Maybe PropertyName)
parseWorkTimeProperty =
  optional $
    setting
      [ help "The property to use to filter by time",
        reader $ eitherReader $ parsePropertyName . T.pack,
        name "time-filter",
        metavar "PROPERTY_NAME"
      ]

data FreeReportSettings = FreeReportSettings
  { freeReportSettingEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeReportSettingLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

instance HasParser FreeReportSettings where
  settingsParser = parseFreeReportSettings

{-# ANN parseFreeReportSettings ("NOCOVER" :: String) #-}
parseFreeReportSettings :: Parser FreeReportSettings
parseFreeReportSettings = do
  freeReportSettingEarliestTimeOfDay <-
    optional $
      setting
        [ help "the earliest time of day to consider free",
          reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
          name "earliest",
          metavar "TIME_OF_DAY"
        ]
  freeReportSettingLatestTimeOfDay <-
    optional $
      setting
        [ help "the latest time of day to consider free",
          reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
          name "latest",
          metavar "TIME_OF_DAY"
        ]
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
