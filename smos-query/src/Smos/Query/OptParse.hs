{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
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

-- combineToInstructions ::
--   Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
-- combineToInstructions c Flags {..} Environment {..} mc = do
--   let hideArchiveWithDefault def mflag = fromMaybe def $ mflag <|> envHideArchive <|> (mc >>= confHideArchive)
--
--   src <-
--     Report.combineToSettings
--       Report.defaultReportSettings
--       flagReportFlags
--       envReportEnvironment
--       (confReportConf <$> mc)
--
--   let colourSettings = getColourSettings $ mc >>= confColourConfiguration
--
--   let settings =
--         Settings
--           { settingColourSettings = colourSettings,
--             settingDirectorySettings = Report.reportSettingDirectorySettings src
--           }
--
--   dispatch <-
--     case c of
--       CommandEntry EntryFlags {..} ->
--         pure $
--           DispatchEntry
--             EntrySettings
--               { entrySetFilter = entryFlagFilter,
--                 entrySetProjection = fromMaybe Report.defaultProjection entryFlagProjection,
--                 entrySetSorter = entryFlagSorter,
--                 entrySetHideArchive = hideArchiveWithDefault HideArchive entryFlagHideArchive,
--                 entrySetOutputFormat = fromMaybe OutputPretty entryFlagOutputFormat
--               }
--       CommandPreparedReport PreparedReportFlags {..} -> do
--         let mprc :: (PreparedReportConfiguration -> Maybe a) -> Maybe a
--             mprc func = mc >>= confPreparedReportConfiguration >>= func
--         pure $
--           DispatchPreparedReport
--             PreparedReportSettings
--               { preparedReportSetReportName = preparedReportFlagReportName,
--                 preparedReportSetAvailableReports = fromMaybe M.empty $ mprc preparedReportConfAvailableReports,
--                 preparedReportSetOutputFormat = fromMaybe OutputPretty preparedReportFlagOutputFormat
--               }
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
--       CommandNext NextFlags {..} ->
--         pure $
--           DispatchNext
--             NextSettings
--               { nextSetFilter = nextFlagFilter,
--                 nextSetHideArchive = hideArchiveWithDefault HideArchive nextFlagHideArchive
--               }
--       CommandOngoing OngoingFlags {..} ->
--         pure $
--           DispatchOngoing
--             OngoingSettings
--               { ongoingSetFilter = ongoingFlagFilter,
--                 ongoingSetHideArchive = hideArchiveWithDefault HideArchive ongoingFlagHideArchive
--               }
--       CommandClock ClockFlags {..} ->
--         pure $
--           DispatchClock
--             ClockSettings
--               { clockSetFilter = clockFlagFilter,
--                 clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags,
--                 clockSetBlock = fromMaybe DayBlock clockFlagBlockFlags,
--                 clockSetOutputFormat = fromMaybe OutputPretty clockFlagOutputFormat,
--                 clockSetClockFormat = case clockFlagClockFormat of
--                   Nothing -> ClockFormatTemporal TemporalMinutesResolution
--                   Just cffs ->
--                     case cffs of
--                       ClockFormatTemporalFlag res ->
--                         ClockFormatTemporal $ fromMaybe TemporalMinutesResolution res
--                       ClockFormatDecimalFlag res ->
--                         ClockFormatDecimal $ fromMaybe (DecimalResolution 2) res,
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
--       CommandProjects ProjectsFlags {..} ->
--         pure $ DispatchProjects ProjectsSettings {projectsSetFilter = projectsFlagFilter}
--       CommandStuck StuckFlags {..} -> do
--         let msc :: (Report.StuckReportSettings -> a) -> a
--             msc func = func $ Report.reportSettingStuckSettings src
--         pure $
--           DispatchStuck
--             StuckSettings
--               { stuckSetFilter = stuckFlagFilter,
--                 stuckSetThreshold = fromMaybe (msc Report.stuckReportSettingThreshold) stuckFlagThreshold
--               }
--       CommandWork WorkFlags {..} -> do
--         let mwac :: (Report.WaitingReportSettings -> a) -> a
--             mwac func = func $ Report.reportSettingWaitingSettings src
--         let msc :: (Report.StuckReportSettings -> a) -> a
--             msc func = func $ Report.reportSettingStuckSettings src
--         let mwc :: (Report.WorkReportSettings -> a) -> a
--             mwc func = func $ Report.reportSettingWorkSettings src
--
--         pure $
--           DispatchWork
--             WorkSettings
--               { workSetContext = workFlagContext,
--                 workSetTime = workFlagTime,
--                 workSetFilter = workFlagFilter,
--                 workSetHideArchive = hideArchiveWithDefault HideArchive workFlagHideArchive,
--                 workSetProjection = fromMaybe (mwc Report.workReportSettingProjection) workFlagProjection,
--                 workSetSorter = mwc Report.workReportSettingSorter <|> workFlagSorter,
--                 workSetWaitingThreshold = fromMaybe (mwac Report.waitingReportSettingThreshold) workFlagWaitingThreshold,
--                 workSetStuckThreshold = fromMaybe (msc Report.stuckReportSettingThreshold) workFlagStuckThreshold,
--                 workSetBaseFilter = mwc Report.workReportSettingBaseFilter,
--                 workSetContexts = mwc Report.workReportSettingContexts,
--                 workSetChecks = mwc Report.workReportSettingChecks,
--                 workSetTimeProperty = mwc Report.workReportSettingTimeProperty
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
-- getEnvironment :: IO (EnvWithConfigFile Environment)
-- getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser
--
-- prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
-- prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser
--
-- environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
-- environmentParser =
--   envWithConfigFileParser $
--     Environment
--       <$> Report.environmentParser
--       <*> optional (Env.var ignoreArchiveReader "IGNORE_ARCHIVE" (Env.help "whether to ignore the archive"))
--   where
--     ignoreArchiveReader = \case
--       "True" -> Right HideArchive
--       "False" -> Right Don'tHideArchive
--       _ -> Left $ Env.UnreadError "Must be 'True' or 'False' if set"
--
-- getArguments :: IO Arguments
-- getArguments = do
--   args <- System.getArgs
--   let result = runArgumentsParser args
--   handleParseResult result
--
-- runArgumentsParser :: [String] -> ParserResult Arguments
-- runArgumentsParser = CLI.execOptionParserPure argParser
--
-- argParser :: ParserInfo Arguments
-- argParser = info (helper <*> parseArgs) help_
--   where
--     help_ = fullDesc <> progDescDoc (Just description)
--     description :: Doc
--     description =
--       Doc.vsep $
--         map Doc.pretty $
--           [ "",
--             "Smos Query Tool version: " <> showVersion version,
--             ""
--           ]
--             ++ readDataVersionsHelpMessage
--
-- parseArgs :: Parser Arguments
-- parseArgs =
--   Arguments
--     <$> parseCommand
--     <*> parseFlagsWithConfigFile parseFlags
--
-- parseCommand :: Parser Command
-- parseCommand =
--   hsubparser $
--     mconcat
--       [ command "entry" parseCommandEntry,
--         command "report" parseCommandReport,
--         command "waiting" parseCommandWaiting,
--         command "next" parseCommandNext,
--         command "ongoing" parseCommandOngoing,
--         command "clock" parseCommandClock,
--         command "agenda" parseCommandAgenda,
--         command "projects" parseCommandProjects,
--         command "stuck" parseCommandStuck,
--         command "work" parseCommandWork,
--         command "free" parseCommandFree,
--         command "log" parseCommandLog,
--         command "stats" parseCommandStats,
--         command "tags" parseCommandTags
--       ]
--
-- parseCommandEntry :: ParserInfo Command
-- parseCommandEntry = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Select entries based on a given filter"
--     parser =
--       CommandEntry
--         <$> ( EntryFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseProjectionArgs
--                 <*> Report.parseSorterArgs
--                 <*> Report.parseHideArchiveFlag
--                 <*> parseOutputFormat
--             )
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
-- parseCommandWork :: ParserInfo Command
-- parseCommandWork = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Show the work overview"
--     parser =
--       CommandWork
--         <$> ( WorkFlags
--                 <$> Report.parseContextNameArg
--                 <*> Report.parseTimeFilterArg
--                 <*> Report.parseFilterOptionsRel
--                 <*> Report.parseProjectionArgs
--                 <*> Report.parseSorterArgs
--                 <*> Report.parseHideArchiveFlag
--                 <*> parseWorkWaitingThresholdFlag
--                 <*> parseWorkStuckThresholdFlag
--             )
--
-- parseWorkWaitingThresholdFlag :: Parser (Maybe Time)
-- parseWorkWaitingThresholdFlag =
--   optional $
--     option
--       (eitherReader $ parseTime . T.pack)
--       ( mconcat
--           [ long "waiting-threshold",
--             metavar "TIME",
--             help "The threshold at which to color waiting entries red"
--           ]
--       )
--
-- parseWorkStuckThresholdFlag :: Parser (Maybe Time)
-- parseWorkStuckThresholdFlag =
--   optional $
--     option
--       (eitherReader $ parseTime . T.pack)
--       ( mconcat
--           [ long "stuck-threshold",
--             metavar "TIME",
--             help "The threshold at which to color stuck projects red"
--           ]
--       )
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
--
-- parseCommandWaiting :: ParserInfo Command
-- parseCommandWaiting = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the \"WAITING\" tasks"
--     parser =
--       CommandWaiting
--         <$> ( WaitingFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseHideArchiveFlag
--                 <*> parseWaitingThresholdFlag
--             )
--
-- parseWaitingThresholdFlag :: Parser (Maybe Time)
-- parseWaitingThresholdFlag =
--   optional $
--     option
--       (eitherReader $ parseTime . T.pack)
--       ( mconcat
--           [ long "threshold",
--             metavar "TIME",
--             help "The threshold at which to color waiting entries red"
--           ]
--       )
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
-- parseCommandProjects :: ParserInfo Command
-- parseCommandProjects = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the projects overview"
--     parser =
--       CommandProjects
--         <$> ( ProjectsFlags
--                 <$> Report.parseProjectFilterArgs
--             )
--
-- parseCommandStuck :: ParserInfo Command
-- parseCommandStuck = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print the stuck projects overview"
--     parser =
--       CommandStuck
--         <$> ( StuckFlags
--                 <$> Report.parseProjectFilterArgs
--                 <*> parseStuckThresholdFlag
--             )
--
-- parseStuckThresholdFlag :: Parser (Maybe Time)
-- parseStuckThresholdFlag =
--   optional $
--     option
--       (eitherReader $ parseTime . T.pack)
--       ( mconcat
--           [ long "threshold",
--             help "The threshold at which to color stuck projects red"
--           ]
--       )
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
-- parseCommandTags :: ParserInfo Command
-- parseCommandTags = info parser modifier
--   where
--     modifier = fullDesc <> progDesc "Print all the tags that are in use"
--     parser =
--       CommandTags
--         <$> ( TagsFlags
--                 <$> Report.parseFilterArgsRel
--                 <*> Report.parseHideArchiveFlag
--             )
--
-- parseFlags :: Parser Flags
-- parseFlags = Flags <$> Report.parseFlags
--
-- parseOutputFormat :: Parser (Maybe OutputFormat)
-- parseOutputFormat =
--   optional
--     ( asum
--         [ flag' OutputPretty $ mconcat [long "pretty", help "pretty text"],
--           flag' OutputYaml $ mconcat [long "yaml", help "Yaml"],
--           flag' OutputJSON $ mconcat [long "json", help "single-line JSON"],
--           flag' OutputJSONPretty $ mconcat [long "pretty-json", help "pretty JSON"]
--         ]
--     )
--
-- data Arguments
--   = Arguments Command (FlagsWithConfigFile Flags)

data Instructions
  = Instructions Dispatch Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

-- data Command
--   = CommandEntry !EntryFlags
--   | CommandPreparedReport !PreparedReportFlags
--   | CommandWaiting !WaitingFlags
--   | CommandNext !NextFlags
--   | CommandOngoing !OngoingFlags
--   | CommandClock !ClockFlags
--   | CommandAgenda !AgendaFlags
--   | CommandProjects !ProjectsFlags
--   | CommandStuck !StuckFlags
--   | CommandWork !WorkFlags
--   | CommandFree !FreeFlags
--   | CommandLog !LogFlags
--   | CommandStats !StatsFlags
--   | CommandTags !TagsFlags
--
-- data EntryFlags = EntryFlags
--   { entryFlagFilter :: !(Maybe EntryFilter),
--     entryFlagProjection :: !(Maybe (NonEmpty Projection)),
--     entryFlagSorter :: !(Maybe Sorter),
--     entryFlagHideArchive :: !(Maybe HideArchive),
--     entryFlagOutputFormat :: !(Maybe OutputFormat)
--   }
--
-- data PreparedReportFlags = PreparedReportFlags
--   { preparedReportFlagReportName :: !(Maybe Text),
--     preparedReportFlagOutputFormat :: !(Maybe OutputFormat)
--   }
--
-- data WaitingFlags = WaitingFlags
--   { waitingFlagFilter :: !(Maybe EntryFilter),
--     waitingFlagHideArchive :: !(Maybe HideArchive),
--     waitingFlagThreshold :: !(Maybe Time)
--   }
--
-- data NextFlags = NextFlags
--   { nextFlagFilter :: !(Maybe EntryFilter),
--     nextFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- data OngoingFlags = OngoingFlags
--   { ongoingFlagFilter :: !(Maybe EntryFilter),
--     ongoingFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- data ClockFlags = ClockFlags
--   { clockFlagFilter :: !(Maybe EntryFilter),
--     clockFlagPeriodFlags :: !(Maybe Period),
--     clockFlagBlockFlags :: !(Maybe TimeBlock),
--     clockFlagOutputFormat :: !(Maybe OutputFormat),
--     clockFlagClockFormat :: !(Maybe ClockFormatFlags),
--     clockFlagReportStyle :: !(Maybe ClockReportStyle),
--     clockFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- data ClockFormatFlags
--   = ClockFormatTemporalFlag !(Maybe TemporalClockResolution)
--   | ClockFormatDecimalFlag !(Maybe DecimalClockResolution)
--
-- data AgendaFlags = AgendaFlags
--   { agendaFlagFilter :: !(Maybe EntryFilter),
--     agendaFlagHistoricity :: !(Maybe AgendaHistoricity),
--     agendaFlagBlock :: !(Maybe TimeBlock),
--     agendaFlagHideArchive :: !(Maybe HideArchive),
--     agendaFlagPeriod :: !(Maybe Period)
--   }
--
-- data ProjectsFlags = ProjectsFlags
--   { projectsFlagFilter :: !(Maybe ProjectFilter)
--   }
--
-- data StuckFlags = StuckFlags
--   { stuckFlagFilter :: !(Maybe ProjectFilter),
--     stuckFlagThreshold :: !(Maybe Time)
--   }
--
-- data WorkFlags = WorkFlags
--   { workFlagContext :: !(Maybe ContextName),
--     workFlagTime :: !(Maybe Time),
--     workFlagFilter :: !(Maybe EntryFilter),
--     workFlagProjection :: !(Maybe (NonEmpty Projection)),
--     workFlagSorter :: !(Maybe Sorter),
--     workFlagHideArchive :: !(Maybe HideArchive),
--     workFlagWaitingThreshold :: !(Maybe Time),
--     workFlagStuckThreshold :: !(Maybe Time)
--   }
--
-- data FreeFlags = FreeFlags
--   { freeFlagPeriodFlags :: !(Maybe Period),
--     freeFlagMinimumTime :: !(Maybe Time),
--     freeFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- data LogFlags = LogFlags
--   { logFlagFilter :: !(Maybe EntryFilter),
--     logFlagPeriodFlags :: !(Maybe Period),
--     logFlagBlockFlags :: !(Maybe TimeBlock),
--     logFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- newtype StatsFlags = StatsFlags
--   { statsFlagPeriodFlags :: Maybe Period
--   }
--
-- data TagsFlags = TagsFlags
--   { tagsFlagFilter :: !(Maybe EntryFilter),
--     tagsFlagHideArchive :: !(Maybe HideArchive)
--   }
--
-- newtype Flags = Flags
--   { flagReportFlags :: Report.Flags
--   }
--
-- data Environment = Environment
--   { envReportEnvironment :: !Report.Environment,
--     envHideArchive :: !(Maybe HideArchive)
--   }
--
-- defaultConfiguration :: Configuration
-- defaultConfiguration =
--   Configuration
--     { confReportConf = Report.defaultConfiguration,
--       confHideArchive = Nothing,
--       confPreparedReportConfiguration = Nothing,
--       confColourConfiguration = Nothing
--     }
--
-- data Configuration = Configuration
--   { confReportConf :: !Report.Configuration,
--     confHideArchive :: !(Maybe HideArchive),
--     confPreparedReportConfiguration :: !(Maybe PreparedReportConfiguration),
--     confColourConfiguration :: !(Maybe ColourConfiguration)
--   }
--   deriving (ToJSON) via (Autodocodec Configuration)
--
-- instance HasCodec Configuration where
--   codec =
--     object "Configuration" $
--       Configuration
--         <$> objectCodec
--           .= confReportConf
--         <*> optionalFieldOrNull "hide-archive" "Whether or not to consider the archive, by default"
--           .= confHideArchive
--         <*> optionalFieldOrNull preparedReportConfigurationKey "Prepared report config"
--           .= confPreparedReportConfiguration
--         <*> colourConfigurationTopLevelObjectCodec
--           .= confColourConfiguration
--
-- preparedReportConfigurationKey :: Text
-- preparedReportConfigurationKey = "report"
--
-- data PreparedReportConfiguration = PreparedReportConfiguration
--   { preparedReportConfAvailableReports :: !(Maybe (Map Text PreparedReport))
--   }
--
-- instance HasCodec PreparedReportConfiguration where
--   codec =
--     object "PreparedReportConfiguration" $
--       PreparedReportConfiguration <$> optionalFieldOrNull "reports" "Custom reports" .= preparedReportConfAvailableReports

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
      [ command "entry" "TODO" $ DispatchEntry <$> settingsParser,
        command "report" "TODO" $ DispatchPreparedReport <$> settingsParser,
        command "waiting" "TODO" $ DispatchWaiting <$> settingsParser,
        command "next" "TODO" $ DispatchNext <$> settingsParser,
        command "ongoing" "TODO" $ DispatchOngoing <$> settingsParser,
        command "clock" "TODO" $ DispatchClock <$> settingsParser,
        command "agenda" "TODO" $ DispatchAgenda <$> settingsParser,
        command "projects" "TODO" $ DispatchProjects <$> settingsParser,
        command "stuck" "TODO" $ DispatchStuck <$> settingsParser,
        command "work" "TODO" $ DispatchWork <$> settingsParser,
        command "free" "TODO" $ DispatchFree <$> settingsParser,
        command "log" "TODO" $ DispatchLog <$> settingsParser,
        command "stats" "TODO" $ DispatchStats <$> settingsParser,
        command "tags" "TODO" $ DispatchTags <$> settingsParser
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
  entrySetFilter <- optional settingsParser
  entrySetProjection <- setting []
  entrySetSorter <- setting []
  entrySetHideArchive <- settingsParser
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
  preparedReportSetReportName <- optional settingsParser
  preparedReportSetAvailableReports <- setting []
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
  waitingSetFilter <- optional settingsParser
  waitingSetHideArchive <- settingsParser
  waitingSetThreshold <- setting []
  pure WaitingSettings {..}

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }

instance HasParser NextSettings where
  settingsParser = parseNextSettings

{-# ANN parseNextSettings ("NOCOVER" :: String) #-}
parseNextSettings :: OptEnvConf.Parser NextSettings
parseNextSettings = do
  nextSetFilter <- optional settingsParser
  nextSetHideArchive <- settingsParser
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
  ongoingSetFilter <- optional settingsParser
  ongoingSetHideArchive <- settingsParser
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
  clockSetFilter <- settingsParser
  clockSetPeriod <- settingsParser
  clockSetBlock <- settingsParser
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
  agendaSetFilter <- optional settingsParser
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
  projectsSetFilter <- optional settingsParser
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
  stuckSetFilter <- optional settingsParser
  stuckSetThreshold <- setting []
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
parseWorkSettings = do
  workSetContext <- optional settingsParser
  workSetContexts <- setting []
  workSetChecks <- setting []
  workSetTime <- optional settingsParser
  workSetTimeProperty <- optional $ setting []
  workSetBaseFilter <- optional $ setting []
  workSetFilter <- optional $ setting []
  workSetProjection <- setting []
  workSetSorter <- optional $ setting []
  workSetHideArchive <- settingsParser
  workSetWaitingThreshold <- setting []
  workSetStuckThreshold <- setting []
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
  freeSetMinimumTime <- setting []
  freeSetHideArchive <- settingsParser
  freeSetEarliestTimeOfDay <- setting []
  freeSetLatestTimeOfDay <- setting []
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
  logSetFilter <- optional settingsParser
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
  tagsSetFilter <- settingsParser
  tagsSetHideArchive <- settingsParser
  pure TagsSettings {..}

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty

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
