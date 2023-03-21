{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Query.OptParse
  ( module Smos.Query.OptParse,
    module Smos.Query.OptParse.Types,
  )
where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative as OptParse
import Options.Applicative.Help.Pretty as Doc
import Paths_smos_query
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Query.OptParse.Types
import Smos.Report.Archive
import Smos.Report.Config
import qualified Smos.Report.OptParse as Report
import Smos.Report.Period
import Smos.Report.Time
import Smos.Report.TimeBlock
import qualified System.Environment as System
import Text.Colour
import Text.Colour.Layout

getInstructions :: IO Instructions
getInstructions = do
  Arguments c flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions c (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions ::
  Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions c Flags {..} Environment {..} mc = do
  let hideArchiveWithDefault def mflag = fromMaybe def $ mflag <|> envHideArchive <|> (mc >>= confHideArchive)

  src <-
    Report.combineToConfig
      defaultReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConf <$> mc)

  let colourSettings = getColourSettings $ mc >>= confColourConfiguration

  let settings =
        Settings
          { settingColourSettings = colourSettings,
            settingDirectoryConfig = smosReportConfigDirectoryConfig src
          }

  dispatch <-
    case c of
      CommandEntry EntryFlags {..} ->
        pure $
          DispatchEntry
            EntrySettings
              { entrySetFilter = entryFlagFilter,
                entrySetProjection = fromMaybe defaultProjection entryFlagProjection,
                entrySetSorter = entryFlagSorter,
                entrySetHideArchive = hideArchiveWithDefault HideArchive entryFlagHideArchive,
                entrySetOutputFormat = fromMaybe OutputPretty entryFlagOutputFormat
              }
      CommandReport ReportFlags {..} -> do
        let mprc :: (PreparedReportConfiguration -> Maybe a) -> Maybe a
            mprc func = mc >>= confPreparedReportConfiguration >>= func
        pure $
          DispatchReport
            ReportSettings
              { reportSetReportName = reportFlagReportName,
                reportSetAvailableReports = fromMaybe M.empty $ mprc preparedReportConfAvailableReports,
                reportSetOutputFormat = fromMaybe OutputPretty reportFlagOutputFormat
              }
      CommandWaiting WaitingFlags {..} -> do
        let mwc :: (WaitingReportConfig -> a) -> a
            mwc func = func $ smosReportConfigWaitingConfig src
        pure $
          DispatchWaiting
            WaitingSettings
              { waitingSetFilter = waitingFlagFilter,
                waitingSetHideArchive = hideArchiveWithDefault HideArchive waitingFlagHideArchive,
                waitingSetThreshold = fromMaybe (mwc waitingReportConfigThreshold) waitingFlagThreshold
              }
      CommandNext NextFlags {..} ->
        pure $
          DispatchNext
            NextSettings
              { nextSetFilter = nextFlagFilter,
                nextSetHideArchive = hideArchiveWithDefault HideArchive nextFlagHideArchive
              }
      CommandClock ClockFlags {..} ->
        pure $
          DispatchClock
            ClockSettings
              { clockSetFilter = clockFlagFilter,
                clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags,
                clockSetBlock = fromMaybe DayBlock clockFlagBlockFlags,
                clockSetOutputFormat = fromMaybe OutputPretty clockFlagOutputFormat,
                clockSetClockFormat = case clockFlagClockFormat of
                  Nothing -> ClockFormatTemporal TemporalMinutesResolution
                  Just cffs ->
                    case cffs of
                      ClockFormatTemporalFlag res ->
                        ClockFormatTemporal $ fromMaybe TemporalMinutesResolution res
                      ClockFormatDecimalFlag res ->
                        ClockFormatDecimal $ fromMaybe (DecimalResolution 2) res,
                clockSetReportStyle = fromMaybe ClockForest clockFlagReportStyle,
                clockSetHideArchive = hideArchiveWithDefault Don'tHideArchive clockFlagHideArchive
              }
      CommandAgenda AgendaFlags {..} -> do
        let period =
              -- Note [Agenda command defaults]
              -- The default here is 'AllTime' for good reason.
              --
              -- You may think that 'Today' is a better default because smos-calendar-import fills up
              -- your agenda too much for it to be useful.
              --
              -- However, as a beginner you want to be able to run smos-query agenda to see your
              -- SCHEDULED and DEADLINE timestamps in the near future.
              -- By the time users figure out how to use smos-calendar-import, they will probably
              -- either already use "smos-query work" or have an alias for 'smos-query agenda --today'
              -- if they need it.
              fromMaybe AllTime agendaFlagPeriod
        let block =
              -- See Note [Agenda command defaults]
              let defaultBlock = case period of
                    AllTime -> OneBlock
                    LastYear -> MonthBlock
                    ThisYear -> MonthBlock
                    NextYear -> MonthBlock
                    LastMonth -> WeekBlock
                    ThisMonth -> WeekBlock
                    NextMonth -> WeekBlock
                    LastWeek -> DayBlock
                    ThisWeek -> DayBlock
                    NextWeek -> DayBlock
                    _ -> OneBlock
               in fromMaybe defaultBlock agendaFlagBlock
        pure $
          DispatchAgenda
            AgendaSettings
              { agendaSetFilter = agendaFlagFilter,
                agendaSetHistoricity = fromMaybe HistoricalAgenda agendaFlagHistoricity,
                agendaSetBlock = block,
                agendaSetHideArchive = hideArchiveWithDefault HideArchive agendaFlagHideArchive,
                agendaSetPeriod = period
              }
      CommandProjects ProjectsFlags {..} ->
        pure $ DispatchProjects ProjectsSettings {projectsSetFilter = projectsFlagFilter}
      CommandStuck StuckFlags {..} -> do
        let msc :: (StuckReportConfig -> a) -> a
            msc func = func $ smosReportConfigStuckConfig src
        pure $
          DispatchStuck
            StuckSettings
              { stuckSetFilter = stuckFlagFilter,
                stuckSetThreshold = fromMaybe (msc stuckReportConfigThreshold) stuckFlagThreshold
              }
      CommandWork WorkFlags {..} -> do
        let mwac :: (WaitingReportConfig -> a) -> a
            mwac func = func $ smosReportConfigWaitingConfig src
        let msc :: (StuckReportConfig -> a) -> a
            msc func = func $ smosReportConfigStuckConfig src
        let mwc :: (WorkReportConfig -> a) -> a
            mwc func = func $ smosReportConfigWorkConfig src

        pure $
          DispatchWork
            WorkSettings
              { workSetContext = workFlagContext,
                workSetTime = workFlagTime,
                workSetFilter = workFlagFilter,
                workSetHideArchive = hideArchiveWithDefault HideArchive workFlagHideArchive,
                workSetProjection = fromMaybe (mwc workReportConfigProjection) workFlagProjection,
                workSetSorter = mwc workReportConfigSorter <|> workFlagSorter,
                workSetWaitingThreshold = fromMaybe (mwac waitingReportConfigThreshold) workFlagWaitingThreshold,
                workSetStuckThreshold = fromMaybe (msc stuckReportConfigThreshold) workFlagStuckThreshold,
                workSetBaseFilter = mwc workReportConfigBaseFilter,
                workSetContexts = mwc workReportConfigContexts,
                workSetChecks = mwc workReportConfigChecks,
                workSetTimeProperty = mwc workReportConfigTimeProperty
              }
      CommandLog LogFlags {..} ->
        pure $
          DispatchLog
            LogSettings
              { logSetFilter = logFlagFilter,
                logSetPeriod = fromMaybe Today logFlagPeriodFlags,
                logSetBlock = fromMaybe DayBlock logFlagBlockFlags,
                logSetHideArchive = hideArchiveWithDefault Don'tHideArchive logFlagHideArchive
              }
      CommandTags TagsFlags {..} ->
        pure $ DispatchTags TagsSettings {tagsSetFilter = tagsFlagFilter}
      CommandStats StatsFlags {..} ->
        pure $
          DispatchStats StatsSettings {statsSetPeriod = fromMaybe AllTime statsFlagPeriodFlags}
  pure $ Instructions dispatch settings

getColourSettings :: Maybe ColourConfiguration -> ColourSettings
getColourSettings mcc =
  ColourSettings
    { colourSettingBackground =
        fromMaybe
          (colourSettingBackground defaultColourSettings)
          (mcc >>= colourConfigurationBackground)
    }

defaultColourSettings :: ColourSettings
defaultColourSettings =
  ColourSettings
    { colourSettingBackground =
        UseTableBackground (Bicolour (Just (Colour8Bit 234)) (Just (Colour8Bit 235)))
    }

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> Report.environmentParser
      <*> optional (Env.var ignoreArchiveReader "IGNORE_ARCHIVE" (Env.help "whether to ignore the archive"))
  where
    ignoreArchiveReader = \case
      "True" -> Right HideArchive
      "False" -> Right Don'tHideArchive
      _ -> Left $ Env.UnreadError "Must be 'True' or 'False' if set"

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Query Tool version: " <> showVersion version,
            ""
          ]
            ++ readDataVersionsHelpMessage

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> parseCommand
    <*> parseFlagsWithConfigFile parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "entry" parseCommandEntry,
        command "report" parseCommandReport,
        command "work" parseCommandWork,
        command "waiting" parseCommandWaiting,
        command "next" parseCommandNext,
        command "clock" parseCommandClock,
        command "agenda" parseCommandAgenda,
        command "projects" parseCommandProjects,
        command "stuck" parseCommandStuck,
        command "log" parseCommandLog,
        command "stats" parseCommandStats,
        command "tags" parseCommandTags
      ]

parseCommandEntry :: ParserInfo Command
parseCommandEntry = info parser modifier
  where
    modifier = fullDesc <> progDesc "Select entries based on a given filter"
    parser =
      CommandEntry
        <$> ( EntryFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parseProjectionArgs
                <*> Report.parseSorterArgs
                <*> Report.parseHideArchiveFlag
                <*> parseOutputFormat
            )

parseCommandReport :: ParserInfo Command
parseCommandReport = info parser modifier
  where
    modifier = fullDesc <> progDesc "Run preconfigured reports"
    parser =
      CommandReport
        <$> ( ReportFlags
                <$> optional
                  ( strArgument
                      ( mconcat
                          [ metavar "REPORT",
                            help "The preconfigured report to run"
                          ]
                      )
                  )
                <*> parseOutputFormat
            )

parseCommandWork :: ParserInfo Command
parseCommandWork = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the work overview"
    parser =
      CommandWork
        <$> ( WorkFlags
                <$> Report.parseContextNameArg
                <*> Report.parseTimeFilterArg
                <*> Report.parseFilterOptionsRel
                <*> Report.parseProjectionArgs
                <*> Report.parseSorterArgs
                <*> Report.parseHideArchiveFlag
                <*> parseWorkWaitingThresholdFlag
                <*> parseWorkStuckThresholdFlag
            )

parseWorkWaitingThresholdFlag :: Parser (Maybe Time)
parseWorkWaitingThresholdFlag =
  optional $
    option
      (eitherReader $ parseTime . T.pack)
      ( mconcat
          [ long "waiting-threshold",
            metavar "TIME",
            help "The threshold at which to color waiting entries red"
          ]
      )

parseWorkStuckThresholdFlag :: Parser (Maybe Time)
parseWorkStuckThresholdFlag =
  optional $
    option
      (eitherReader $ parseTime . T.pack)
      ( mconcat
          [ long "stuck-threshold",
            metavar "TIME",
            help "The threshold at which to color stuck projects red"
          ]
      )

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the \"WAITING\" tasks"
    parser =
      CommandWaiting
        <$> ( WaitingFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parseHideArchiveFlag
                <*> parseWaitingThresholdFlag
            )

parseWaitingThresholdFlag :: Parser (Maybe Time)
parseWaitingThresholdFlag =
  optional $
    option
      (eitherReader $ parseTime . T.pack)
      ( mconcat
          [ long "threshold",
            metavar "TIME",
            help "The threshold at which to color waiting entries red"
          ]
      )

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the next actions"
    parser =
      CommandNext
        <$> ( NextFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parseHideArchiveFlag
            )

parseCommandClock :: ParserInfo Command
parseCommandClock = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the clock table"
    parser =
      CommandClock
        <$> ( ClockFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parsePeriod
                <*> Report.parseTimeBlock
                <*> parseOutputFormat
                <*> parseClockFormatFlags
                <*> parseClockReportStyle
                <*> Report.parseHideArchiveFlag
            )

parseClockFormatFlags :: Parser (Maybe ClockFormatFlags)
parseClockFormatFlags =
  optional
    ( flag' ClockFormatTemporalFlag (long "temporal-resolution") <*> parseTemporalClockResolution
        <|> flag' ClockFormatDecimalFlag (long "decimal-resolution") <*> parseDecimalClockResolution
    )

parseTemporalClockResolution :: Parser (Maybe TemporalClockResolution)
parseTemporalClockResolution =
  optional
    ( flag' TemporalSecondsResolution (long "seconds-resolution")
        <|> flag' TemporalMinutesResolution (long "minutes-resolution")
        <|> flag' TemporalHoursResolution (long "hours-resolution")
    )

parseDecimalClockResolution :: Parser (Maybe DecimalClockResolution)
parseDecimalClockResolution =
  optional
    ( flag' DecimalQuarterResolution (long "quarters-resolution")
        <|> (flag' DecimalResolution (long "resolution") <*> argument auto (help "significant digits"))
        <|> flag' DecimalHoursResolution (long "hours-resolution")
    )

parseClockReportStyle :: Parser (Maybe ClockReportStyle)
parseClockReportStyle =
  optional (flag' ClockForest (long "forest") <|> flag' ClockFlat (long "flat"))

parseCommandAgenda :: ParserInfo Command
parseCommandAgenda = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the agenda"
    parser =
      CommandAgenda
        <$> ( AgendaFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parseHistoricityFlag
                <*> Report.parseTimeBlock
                <*> Report.parseHideArchiveFlag
                <*> Report.parsePeriod
            )

parseCommandProjects :: ParserInfo Command
parseCommandProjects = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the projects overview"
    parser =
      CommandProjects
        <$> ( ProjectsFlags
                <$> Report.parseProjectFilterArgs
            )

parseCommandStuck :: ParserInfo Command
parseCommandStuck = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the stuck projects overview"
    parser =
      CommandStuck
        <$> ( StuckFlags
                <$> Report.parseProjectFilterArgs
                <*> parseStuckThresholdFlag
            )

parseStuckThresholdFlag :: Parser (Maybe Time)
parseStuckThresholdFlag =
  optional $
    option
      (eitherReader $ parseTime . T.pack)
      ( mconcat
          [ long "threshold",
            help "The threshold at which to color stuck projects red"
          ]
      )

parseCommandLog :: ParserInfo Command
parseCommandLog = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print a log of what has happened."
    parser =
      CommandLog
        <$> ( LogFlags
                <$> Report.parseFilterArgsRel
                <*> Report.parsePeriod
                <*> Report.parseTimeBlock
                <*> Report.parseHideArchiveFlag
            )

parseCommandStats :: ParserInfo Command
parseCommandStats = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the stats actions and warn if a file does not have one."
    parser =
      CommandStats
        <$> ( StatsFlags
                <$> Report.parsePeriod
            )

parseCommandTags :: ParserInfo Command
parseCommandTags = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print all the tags that are in use"
    parser =
      CommandTags
        <$> ( TagsFlags
                <$> Report.parseFilterArgsRel
            )

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseFlags

parseOutputFormat :: Parser (Maybe OutputFormat)
parseOutputFormat =
  optional
    ( asum
        [ flag' OutputPretty $ mconcat [long "pretty", help "pretty text"],
          flag' OutputYaml $ mconcat [long "yaml", help "Yaml"],
          flag' OutputJSON $ mconcat [long "json", help "single-line JSON"],
          flag' OutputJSONPretty $ mconcat [long "pretty-json", help "pretty JSON"]
        ]
    )
