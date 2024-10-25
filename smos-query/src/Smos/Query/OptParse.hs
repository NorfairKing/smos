{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Query.OptParse
  ( module Smos.Query.OptParse,
    module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Directory.ShouldPrint,
  )
where

import Autodocodec
import Data.Aeson (ToJSON (..))
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Version
import OptEnvConf
import Paths_smos_query (version)
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Directory.ShouldPrint
import Smos.Report.Agenda.Types
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.OptParse (ContextName)
import qualified Smos.Report.OptParse as Report
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Report
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions =
  runSettingsParser version $
    unlines $
      concat
        [ [ "Smos' query tool",
            ""
          ],
          readDataVersionsHelpMessage
        ]

--             ClockSettings
--                 clockSetReportStyle = fromMaybe ClockForest clockFlagReportStyle,
--                 clockSetHideArchive = hideArchiveWithDefault Don'tHideArchive clockFlagHideArchive
--               }
--       CommandAgenda AgendaFlags {..} -> do
--         let period =
--               -- Note [Agenda command defaults]
--               -- The default here is 'AllTime' for good reason.
--               --
--               -- You may think that 'Today' is a better default because smos-calendar-import fills up
--               -- your agenda too much for it to be useful.
--               --
--               -- However, as a beginner you want to be able to run smos-query agenda to see your
--               -- SCHEDULED and DEADLINE timestamps in the near future.
--               -- By the time users figure out how to use smos-calendar-import, they will probably
--               -- either already use "smos-query work" or have an alias for 'smos-query agenda --today'
--               -- if they need it.
--               fromMaybe AllTime agendaFlagPeriod
--         let block =
--               -- See Note [Agenda command defaults]
--               let defaultBlock = case period of
--                     AllTime -> OneBlock
--                     LastYear -> MonthBlock
--                     ThisYear -> MonthBlock
--                     NextYear -> MonthBlock
--                     LastMonth -> WeekBlock
--                     ThisMonth -> WeekBlock
--                     NextMonth -> WeekBlock
--                     LastWeek -> DayBlock
--                     ThisWeek -> DayBlock
--                     NextWeek -> DayBlock
--                     _ -> OneBlock
--                in fromMaybe defaultBlock agendaFlagBlock
--         pure $
--           DispatchAgenda
--             AgendaSettings
--               { agendaSetFilter = agendaFlagFilter,
--                 agendaSetHistoricity = fromMaybe HistoricalAgenda agendaFlagHistoricity,
--                 agendaSetBlock = block,
--                 agendaSetHideArchive = hideArchiveWithDefault HideArchive agendaFlagHideArchive,
--                 agendaSetPeriod = period
--               }
--       CommandFree FreeFlags {..} -> do
--         let mfc :: (Report.FreeReportSettings -> a) -> a
--             mfc func = func $ Report.reportSettingFreeSettings src
--         pure $
--           DispatchFree
--             FreeSettings
--               { freeSetPeriod = fromMaybe ComingWeek freeFlagPeriodFlags,
--                 freeSetMinimumTime = freeFlagMinimumTime,
--                 freeSetHideArchive = hideArchiveWithDefault HideArchive freeFlagHideArchive,
--                 freeSetEarliestTimeOfDay = mfc Report.freeReportSettingEarliestTimeOfDay,
--                 freeSetLatestTimeOfDay = mfc Report.freeReportSettingLatestTimeOfDay
--               }
--       CommandLog LogFlags {..} ->
--         pure $
--           DispatchLog
--             LogSettings
--               { logSetFilter = logFlagFilter,
--                 logSetPeriod = fromMaybe Today logFlagPeriodFlags,
--                 logSetBlock = fromMaybe DayBlock logFlagBlockFlags,
--                 logSetHideArchive = hideArchiveWithDefault Don'tHideArchive logFlagHideArchive
--               }
--       CommandTags TagsFlags {..} ->
--         pure $
--           DispatchTags
--             TagsSettings
--               { tagsSetFilter = tagsFlagFilter,
--                 tagsSetHideArchive = hideArchiveWithDefault HideArchive tagsFlagHideArchive
--               }
--       CommandStats StatsFlags {..} ->
--         pure $
--           DispatchStats StatsSettings {statsSetPeriod = fromMaybe AllTime statsFlagPeriodFlags}
--   pure $ Instructions dispatch settings
--
--
-- parseCommandReport :: ParserInfo Command
-- parseCommandReport = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Run prepared reports"
--     parser =
--       CommandPreparedReport
--         <$> ( PreparedReportFlags
--                 <$> optional
--                   ( strArgument
--                       ( mconcat
--                           [ metavar "REPORT",
--                             help "The prepared report to run"
--                           ]
--                       )
--                   )
--                 <*> parseOutputFormat
--             )
--
--
-- parseCommandFree :: ParserInfo Command
-- parseCommandFree = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Find a free slot for a meeting"
--     parser =
--       CommandFree
--         <$> ( FreeFlags
--                 <$> Report.parsePeriod
--                 <*> parseMinimumTimeFlag
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseMinimumTimeFlag :: Parser (Maybe Time)
-- parseMinimumTimeFlag =
--   optional $
--     option
--       (eitherReader $ parseTime . T.pack)
--       ( mconcat
--           [ long "time",
--             short 't',
--             metavar "TIME",
--             help "The minimum amount of free time to show a free time slot"
--           ]
--       )

parseWaitingThresholdOption :: Parser Time
parseWaitingThresholdOption =
  setting
    [ help "The threshold at which to color waiting entries red",
      option,
      reader $ eitherReader $ parseTime . T.pack,
      long "waiting-threshold",
      metavar "TIME",
      value Report.defaultWaitingThreshold
    ]

--
-- parseCommandNext :: ParserInfo Command
-- parseCommandNext = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the next actions"
--     parser =
--       CommandNext
--         <$> ( NextFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseCommandOngoing :: ParserInfo Command
-- parseCommandOngoing = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the ongoing entries"
--     parser =
--       CommandOngoing
--         <$> ( OngoingFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseCommandClock :: ParserInfo Command
-- parseCommandClock = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the clock table"
--     parser =
--       CommandClock
--         <$> ( ClockFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parsePeriod
--                 <*> Report.parseTimeBlock
--                 <*> parseOutputFormat
--                 <*> parseClockFormatFlags
--                 <*> parseClockReportStyle
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseClockFormatFlags :: Parser (Maybe ClockFormatFlags)
-- parseClockFormatFlags =
--   optional
--     ( flag' ClockFormatTemporalFlag (long "temporal-resolution")
--         <*> parseTemporalClockResolution
--           <|> flag' ClockFormatDecimalFlag (long "decimal-resolution")
--         <*> parseDecimalClockResolution
--     )
--
-- parseTemporalClockResolution :: Parser (Maybe TemporalClockResolution)
-- parseTemporalClockResolution =
--   optional
--     ( flag' TemporalSecondsResolution (long "seconds-resolution")
--         <|> flag' TemporalMinutesResolution (long "minutes-resolution")
--         <|> flag' TemporalHoursResolution (long "hours-resolution")
--     )
--
-- parseDecimalClockResolution :: Parser (Maybe DecimalClockResolution)
-- parseDecimalClockResolution =
--   optional
--     ( flag' DecimalQuarterResolution (long "quarters-resolution")
--         <|> (flag' DecimalResolution (long "resolution") <*> argument auto (help "significant digits"))
--         <|> flag' DecimalHoursResolution (long "hours-resolution")
--     )
--
-- parseClockReportStyle :: Parser (Maybe ClockReportStyle)
-- parseClockReportStyle =
--   optional (flag' ClockForest (long "forest") <|> flag' ClockFlat (long "flat"))
--
-- parseCommandAgenda :: ParserInfo Command
-- parseCommandAgenda = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the agenda"
--     parser =
--       CommandAgenda
--         <$> ( AgendaFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseHistoricityFlag
--                 <*> Report.parseTimeBlock
--                 <*> Report.parseHideArchiveFlag
--                 <*> Report.parsePeriod
--             )
--
parseStuckThresholdOption :: Parser Time
parseStuckThresholdOption =
  setting
    [ help "The threshold at which to color stuck projects red",
      option,
      reader $ eitherReader $ parseTime . T.pack,
      long "stuck-threshold",
      metavar "TIME",
      value Report.defaultStuckThreshold
    ]

--
-- parseCommandLog :: ParserInfo Command
-- parseCommandLog = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print a log of what has happened."
--     parser =
--       CommandLog
--         <$> ( LogFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parsePeriod
--                 <*> Report.parseTimeBlock
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseCommandStats :: ParserInfo Command
-- parseCommandStats = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the stats actions and warn if a file does not have one."
--     parser =
--       CommandStats
--         <$> ( StatsFlags
--                 <$> Report.parsePeriod
--             )
--

data Instructions
  = Instructions Dispatch Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchEntry !EntrySettings
  | DispatchPreparedReport !PreparedReportSettings
  | DispatchWaiting !WaitingSettings
  | DispatchNext !NextSettings
  | DispatchOngoing !OngoingSettings
  | DispatchClock !ClockSettings
  | DispatchAgenda !AgendaSettings
  | DispatchProjects !ProjectsSettings
  | DispatchStuck !StuckSettings
  | DispatchWork !WorkSettings
  | DispatchFree !FreeSettings
  | DispatchLog !LogSettings
  | DispatchStats !StatsSettings
  | DispatchTags !TagsSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "entry" "Run a custom report with given filter, sorter, ..." $ DispatchEntry <$> settingsParser,
        command "report" "Run a prepared report" $ DispatchPreparedReport <$> settingsParser,
        command "waiting" "Run the waiting report" $ DispatchWaiting <$> settingsParser,
        command "next" "Run the next actions report" $ DispatchNext <$> settingsParser,
        command "ongoing" "TODO" $ DispatchOngoing <$> settingsParser,
        command "clock" "TODO" $ DispatchClock <$> settingsParser,
        command "agenda" "TODO" $ DispatchAgenda <$> settingsParser,
        command "projects" "Run the projects overview" $ DispatchProjects <$> settingsParser,
        command "stuck" "Run the stuck projects report" $ DispatchStuck <$> settingsParser,
        command "work" "Run the work report" $ DispatchWork <$> settingsParser,
        command "free" "TODO" $ DispatchFree <$> settingsParser,
        command "log" "TODO" $ DispatchLog <$> settingsParser,
        command "stats" "TODO" $ DispatchStats <$> settingsParser,
        command "tags" "List all the tags that are in use" $ DispatchTags <$> settingsParser
      ]

data EntrySettings = EntrySettings
  { entrySetFilter :: !(Maybe EntryFilter),
    entrySetProjection :: !(NonEmpty Projection),
    entrySetSorter :: !(Maybe Sorter),
    entrySetHideArchive :: !HideArchive,
    entrySetOutputFormat :: !OutputFormat
  }

instance HasParser EntrySettings where
  settingsParser = parseEntrySettings

{-# ANN parseEntrySettings ("NOCOVER" :: String) #-}
parseEntrySettings :: OptEnvConf.Parser EntrySettings
parseEntrySettings = do
  entrySetFilter <- optional Report.parseFilterArgs
  entrySetProjection <- Report.parseProjectionOptions
  entrySetSorter <- optional Report.parseSorterOptions
  entrySetHideArchive <- withDefault HideArchive settingsParser
  entrySetOutputFormat <- settingsParser
  pure EntrySettings {..}

data PreparedReportSettings = PreparedReportSettings
  { preparedReportSetReportName :: !(Maybe Text),
    preparedReportSetAvailableReports :: !(Map Text PreparedReport),
    preparedReportSetOutputFormat :: !OutputFormat
  }

instance HasParser PreparedReportSettings where
  settingsParser = parsePreparedReportSettings

{-# ANN parsePreparedReportSettings ("NOCOVER" :: String) #-}
parsePreparedReportSettings :: OptEnvConf.Parser PreparedReportSettings
parsePreparedReportSettings = do
  preparedReportSetReportName <-
    optional $
      setting
        [ help "name of the report",
          argument,
          reader str,
          metavar "NAME"
        ]
  preparedReportSetAvailableReports <-
    setting
      [ help "available reports",
        conf "reports",
        value M.empty
      ]
  preparedReportSetOutputFormat <- settingsParser
  pure PreparedReportSettings {..}

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilter),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Time
  }

instance HasParser WaitingSettings where
  settingsParser = parseWaitingSettings

{-# ANN parseWaitingSettings ("NOCOVER" :: String) #-}
parseWaitingSettings :: OptEnvConf.Parser WaitingSettings
parseWaitingSettings = do
  waitingSetFilter <- optional Report.parseFilterArgs
  waitingSetHideArchive <- settingsParser
  waitingSetThreshold <- parseWaitingThresholdOption
  pure WaitingSettings {..}

--       CommandWaiting WaitingFlags {..} -> do
--         let mwc :: (Report.WaitingReportSettings -> a) -> a
--             mwc func = func $ Report.reportSettingWaitingSettings src
--         pure $
--           DispatchWaiting
--             WaitingSettings
--               { waitingSetFilter = waitingFlagFilter,
--                 waitingSetHideArchive = hideArchiveWithDefault HideArchive waitingFlagHideArchive,
--                 waitingSetThreshold = fromMaybe (mwc Report.waitingReportSettingThreshold) waitingFlagThreshold
--               }

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }

instance HasParser NextSettings where
  settingsParser = parseNextSettings

{-# ANN parseNextSettings ("NOCOVER" :: String) #-}
parseNextSettings :: OptEnvConf.Parser NextSettings
parseNextSettings = do
  nextSetFilter <- optional Report.parseFilterArgs
  nextSetHideArchive <- withDefault HideArchive settingsParser
  pure NextSettings {..}

data OngoingSettings = OngoingSettings
  { ongoingSetFilter :: !(Maybe EntryFilter),
    ongoingSetHideArchive :: !HideArchive
  }

instance HasParser OngoingSettings where
  settingsParser = parseOngoingSettings

{-# ANN parseOngoingSettings ("NOCOVER" :: String) #-}
parseOngoingSettings :: OptEnvConf.Parser OngoingSettings
parseOngoingSettings = do
  ongoingSetFilter <- optional Report.parseFilterArgs
  ongoingSetHideArchive <- withDefault HideArchive settingsParser
  pure OngoingSettings {..}

data ClockSettings = ClockSettings
  { clockSetFilter :: !(Maybe EntryFilter),
    clockSetPeriod :: !Period,
    clockSetBlock :: !TimeBlock,
    clockSetOutputFormat :: !OutputFormat,
    clockSetClockFormat :: !ClockFormat,
    clockSetReportStyle :: !ClockReportStyle,
    clockSetHideArchive :: !HideArchive
  }

instance HasParser ClockSettings where
  settingsParser = parseClockSettings

{-# ANN parseClockSettings ("NOCOVER" :: String) #-}
parseClockSettings :: OptEnvConf.Parser ClockSettings
parseClockSettings = do
  clockSetFilter <- optional Report.parseFilterArgs
  clockSetPeriod <- withDefault AllTime settingsParser
  clockSetBlock <- withDefault OneBlock settingsParser
  clockSetOutputFormat <- settingsParser
  clockSetClockFormat <- settingsParser
  clockSetReportStyle <- settingsParser
  clockSetHideArchive <- settingsParser
  pure ClockSettings {..}

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: !(Maybe EntryFilter),
    agendaSetHistoricity :: !AgendaHistoricity,
    agendaSetBlock :: !TimeBlock,
    agendaSetHideArchive :: !HideArchive,
    agendaSetPeriod :: !Period
  }

instance HasParser AgendaSettings where
  settingsParser = parseAgendaSettings

{-# ANN parseAgendaSettings ("NOCOVER" :: String) #-}
parseAgendaSettings :: OptEnvConf.Parser AgendaSettings
parseAgendaSettings = do
  agendaSetFilter <- optional Report.parseFilterArgs
  agendaSetHistoricity <- settingsParser
  agendaSetBlock <- settingsParser
  agendaSetHideArchive <- settingsParser
  agendaSetPeriod <- settingsParser
  pure AgendaSettings {..}

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: !(Maybe ProjectFilter)
  }

instance HasParser ProjectsSettings where
  settingsParser = parseProjectsSettings

{-# ANN parseProjectsSettings ("NOCOVER" :: String) #-}
parseProjectsSettings :: OptEnvConf.Parser ProjectsSettings
parseProjectsSettings = do
  projectsSetFilter <- Report.parseProjectFilterArgs
  pure ProjectsSettings {..}

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Time
  }

instance HasParser StuckSettings where
  settingsParser = parseStuckSettings

{-# ANN parseStuckSettings ("NOCOVER" :: String) #-}
parseStuckSettings :: OptEnvConf.Parser StuckSettings
parseStuckSettings = do
  stuckSetFilter <- Report.parseProjectFilterArgs
  stuckSetThreshold <- parseStuckThresholdOption
  pure StuckSettings {..}

data WorkSettings = WorkSettings
  { workSetContext :: !(Maybe ContextName),
    workSetContexts :: !(Map ContextName EntryFilter),
    workSetChecks :: !(Set EntryFilter),
    workSetTime :: !(Maybe Time),
    workSetTimeProperty :: !(Maybe PropertyName),
    workSetBaseFilter :: !(Maybe EntryFilter),
    workSetFilter :: !(Maybe EntryFilter),
    workSetProjection :: !(NonEmpty Projection),
    workSetSorter :: !(Maybe Sorter),
    workSetHideArchive :: !HideArchive,
    workSetWaitingThreshold :: !Time,
    workSetStuckThreshold :: !Time
  }

instance HasParser WorkSettings where
  settingsParser = parseWorkSettings

{-# ANN parseWorkSettings ("NOCOVER" :: String) #-}
parseWorkSettings :: OptEnvConf.Parser WorkSettings
parseWorkSettings = subEnv_ "work" $ subConfig_ "work" $ do
  workSetContext <- optional settingsParser
  workSetContexts <- Report.parseWorkContexts
  workSetChecks <- Report.parseWorkChecks
  workSetTime <- optional settingsParser
  workSetTimeProperty <- Report.parseWorkTimeProperty
  workSetBaseFilter <- Report.parseWorkBaseFilter
  workSetFilter <- optional Report.parseFilterOptions
  workSetProjection <- Report.parseProjectionOptions
  workSetSorter <- optional Report.parseSorterOptions
  workSetHideArchive <- withDefault HideArchive settingsParser
  workSetWaitingThreshold <- parseWaitingThresholdOption
  workSetStuckThreshold <- parseStuckThresholdOption
  pure WorkSettings {..}

data FreeSettings = FreeSettings
  { freeSetPeriod :: !Period,
    freeSetMinimumTime :: !(Maybe Time),
    freeSetHideArchive :: !HideArchive,
    freeSetEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeSetLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

instance HasParser FreeSettings where
  settingsParser = parseFreeSettings

{-# ANN parseFreeSettings ("NOCOVER" :: String) #-}
parseFreeSettings :: OptEnvConf.Parser FreeSettings
parseFreeSettings = do
  freeSetPeriod <- settingsParser
  freeSetMinimumTime <-
    optional $
      setting
        [ help "Minimum time required",
          argument,
          reader $ eitherReader $ parseTime . T.pack,
          metavar "TIME"
        ]
  freeSetHideArchive <- settingsParser
  freeSetEarliestTimeOfDay <-
    optional $
      setting
        [ help "Earliest time of day",
          option,
          reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
          reader auto,
          long "earliest",
          metavar "TIME_OF_DAY"
        ]
  freeSetLatestTimeOfDay <-
    optional $
      setting
        [ help "Latest time of day",
          option,
          reader $ maybeReader $ parseTimeM True defaultTimeLocale "%H:%M",
          reader auto,
          long "latest",
          metavar "TIME_OF_DAY"
        ]
  pure FreeSettings {..}

data LogSettings = LogSettings
  { logSetFilter :: !(Maybe EntryFilter),
    logSetPeriod :: !Period,
    logSetBlock :: !TimeBlock,
    logSetHideArchive :: !HideArchive
  }

instance HasParser LogSettings where
  settingsParser = parseLogSettings

{-# ANN parseLogSettings ("NOCOVER" :: String) #-}
parseLogSettings :: OptEnvConf.Parser LogSettings
parseLogSettings = do
  logSetFilter <- optional Report.parseFilterArgs
  logSetPeriod <- settingsParser
  logSetBlock <- settingsParser
  logSetHideArchive <- settingsParser
  pure LogSettings {..}

data StatsSettings = StatsSettings
  { statsSetPeriod :: !Period
  }

instance HasParser StatsSettings where
  settingsParser = parseStatsSettings

{-# ANN parseStatsSettings ("NOCOVER" :: String) #-}
parseStatsSettings :: OptEnvConf.Parser StatsSettings
parseStatsSettings = do
  statsSetPeriod <- settingsParser
  pure StatsSettings {..}

data TagsSettings = TagsSettings
  { tagsSetFilter :: !(Maybe EntryFilter),
    tagsSetHideArchive :: !HideArchive
  }

instance HasParser TagsSettings where
  settingsParser = parseTagsSettings

{-# ANN parseTagsSettings ("NOCOVER" :: String) #-}
parseTagsSettings :: OptEnvConf.Parser TagsSettings
parseTagsSettings = do
  tagsSetFilter <- optional Report.parseFilterArgs
  tagsSetHideArchive <- settingsParser
  pure TagsSettings {..}

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty

instance HasParser OutputFormat where
  settingsParser =
    withShownDefault OutputPretty "pretty" $
      choice
        [ setting
            [ help "pretty text",
              switch OutputPretty,
              long "pretty"
            ],
          setting
            [ help "Yaml",
              switch OutputYaml,
              long "yaml"
            ],
          setting
            [ help "single-line JSON",
              switch OutputJSON,
              long "json"
            ],
          setting
            [ help "pretty JSON",
              switch OutputJSONPretty,
              long "pretty-json"
            ]
        ]

data Settings = Settings
  { settingDirectorySettings :: !DirectorySettings,
    settingColourSettings :: !ColourSettings
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  settingDirectorySettings <- settingsParser
  settingColourSettings <- settingsParser
  pure Settings {..}
