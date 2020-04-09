{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.OptParse where

import Control.Arrow
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time hiding (parseTime)
import Text.Read (readMaybe)

import qualified System.Environment as System
import System.Exit

import Options.Applicative

import qualified Smos.Report.OptParse as Report

import Smos.Report.Comparison
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

import Smos.Query.Config
import Smos.Query.OptParse.Types

getInstructions :: SmosQueryConfig -> IO Instructions
getInstructions sqc = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions sqc args env config

combineToInstructions ::
     SmosQueryConfig -> Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions SmosQueryConfig {..} (Arguments c Flags {..}) Environment {..} mc =
  Instructions <$> getDispatch <*> getSettings
  where
    hideArchiveWithDefault def mflag =
      fromMaybe def $ mflag <|> envHideArchive <|> (mc >>= confHideArchive)
    getDispatch =
      case c of
        CommandEntry EntryFlags {..} ->
          pure $
          DispatchEntry
            EntrySettings
              { entrySetFilter = entryFlagFilter
              , entrySetProjection = fromMaybe defaultProjection entryFlagProjection
              , entrySetSorter = entryFlagSorter
              , entrySetHideArchive = hideArchiveWithDefault HideArchive entryFlagHideArchive
              }
        CommandReport ReportFlags {..} ->
          pure $
          DispatchReport
            ReportSettings
              { reportSetReportName = reportFlagReportName
              , reportSetAvailableReports = fromMaybe M.empty $ mc >>= confAvailableReports
              }
        CommandWaiting WaitingFlags {..} -> do
          let mwc func = mc >>= confWaitingConfiguration >>= func
          pure $
            DispatchWaiting
              WaitingSettings
                { waitingSetFilter = waitingFlagFilter
                , waitingSetHideArchive = hideArchiveWithDefault HideArchive waitingFlagHideArchive
                , waitingSetThreshold =
                    fromMaybe 7 $ waitingFlagThreshold <|> mwc waitingConfThreshold
                }
        CommandNext NextFlags {..} ->
          pure $
          DispatchNext
            NextSettings
              { nextSetFilter = nextFlagFilter
              , nextSetHideArchive = hideArchiveWithDefault HideArchive nextFlagHideArchive
              }
        CommandClock ClockFlags {..} ->
          pure $
          DispatchClock
            ClockSettings
              { clockSetFilter = clockFlagFilter
              , clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags
              , clockSetBlock = fromMaybe OneBlock clockFlagBlockFlags
              , clockSetOutputFormat = fromMaybe OutputPretty clockFlagOutputFormat
              , clockSetClockFormat =
                  case clockFlagClockFormat of
                    Nothing -> ClockFormatTemporal TemporalMinutesResolution
                    Just cffs ->
                      case cffs of
                        ClockFormatTemporalFlag res ->
                          ClockFormatTemporal $ fromMaybe TemporalMinutesResolution res
                        ClockFormatDecimalFlag res ->
                          ClockFormatDecimal $ fromMaybe (DecimalResolution 2) res
              , clockSetReportStyle = fromMaybe ClockForest clockFlagReportStyle
              , clockSetHideArchive = hideArchiveWithDefault Don'tHideArchive clockFlagHideArchive
              }
        CommandAgenda AgendaFlags {..} ->
          pure $
          DispatchAgenda
            AgendaSettings
              { agendaSetFilter = agendaFlagFilter
              , agendaSetHistoricity = fromMaybe HistoricalAgenda agendaFlagHistoricity
              , agendaSetBlock = fromMaybe OneBlock agendaFlagBlock
              , agendaSetHideArchive = hideArchiveWithDefault HideArchive agendaFlagHideArchive
              }
        CommandWork WorkFlags {..} -> do
          let wc func = func <$> (mc >>= confWorkConfiguration)
              mwc func = mc >>= confWorkConfiguration >>= func
              combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
              combineMaybe f m1 m2 =
                case (m1, m2) of
                  (Nothing, Nothing) -> Nothing
                  (Just a, Nothing) -> Just a
                  (Nothing, Just a) -> Just a
                  (Just a1, Just a2) -> Just $ f a1 a2
          mtf <-
            case (workFlagTimeFilter, mwc workConfTimeFilterProperty) of
              (_, Nothing) -> die "No time filter property configured."
              (tf, Just pn) ->
                pure $
                Just $
                FilterEntryProperties $
                FilterMapVal pn $ FilterMaybe False $ FilterPropertyTime $ FilterMaybe False tf
          pure $
            DispatchWork
              WorkSettings
                { workSetContext = workFlagContext
                , workSetTimeFilter = mtf
                , workSetFilter = workFlagFilter
                , workSetChecks = fromMaybe S.empty $ wc workConfChecks
                , workSetProjection =
                    fromMaybe defaultProjection $
                    combineMaybe (<>) (mwc workConfProjection) workFlagProjection
                , workSetSorter = mwc workConfSorter <|> workFlagSorter
                , workSetHideArchive = hideArchiveWithDefault HideArchive workFlagHideArchive
                }
        CommandProjects ProjectsFlags {..} ->
          pure $ DispatchProjects ProjectsSettings {projectsSetFilter = projectsFlagFilter}
        CommandStuck StuckFlags {..} ->
          pure $ DispatchStuck StuckSettings {stuckSetFilter = stuckFlagFilter}
        CommandLog LogFlags {..} ->
          pure $
          DispatchLog
            LogSettings
              { logSetFilter = logFlagFilter
              , logSetPeriod = fromMaybe AllTime logFlagPeriodFlags
              , logSetBlock = fromMaybe OneBlock logFlagBlockFlags
              , logSetHideArchive = hideArchiveWithDefault Don'tHideArchive logFlagHideArchive
              }
        CommandTags TagsFlags {..} ->
          pure $ DispatchTags TagsSettings {tagsSetFilter = tagsFlagFilter}
        CommandStats StatsFlags {..} ->
          pure $
          DispatchStats StatsSettings {statsSetPeriod = fromMaybe AllTime statsFlagPeriodFlags}
    getSettings = do
      src <-
        Report.combineToConfig
          smosQueryConfigReportConfig
          flagReportFlags
          envReportEnvironment
          (confReportConf <$> mc)
      pure $ SmosQueryConfig {smosQueryConfigReportConfig = src}

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let getSmosEnv :: String -> Maybe String
      getSmosEnv key = ("SMOS_" ++ key) `lookup` env
      readSmosEnv :: Read a => String -> Maybe a
      readSmosEnv key = getSmosEnv key >>= readMaybe
  envReportEnvironment <- Report.getEnvironment
  let envHideArchive =
        readSmosEnv "IGNORE_ARCHIVE" <&> \case
          True -> Don'tHideArchive
          False -> HideArchive
  pure Environment {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  Report.getConfiguration flagReportFlags envReportEnvironment

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-query"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
  mconcat
    [ command "entry" parseCommandEntry
    , command "report" parseCommandReport
    , command "work" parseCommandWork
    , command "waiting" parseCommandWaiting
    , command "next" parseCommandNext
    , command "clock" parseCommandClock
    , command "agenda" parseCommandAgenda
    , command "projects" parseCommandProjects
    , command "stuck" parseCommandStuck
    , command "log" parseCommandLog
    , command "stats" parseCommandStats
    , command "tags" parseCommandTags
    ]

parseCommandEntry :: ParserInfo Command
parseCommandEntry = info parser modifier
  where
    modifier = fullDesc <> progDesc "Select entries based on a given filter"
    parser =
      CommandEntry <$>
      (EntryFlags <$> parseFilterArgs <*> parseProjectionArgs <*> parseSorterArgs <*>
       parseHideArchiveFlag)

parseCommandReport :: ParserInfo Command
parseCommandReport = info parser modifier
  where
    modifier = fullDesc <> progDesc "Run preconfigure reports"
    parser =
      CommandReport <$>
      (ReportFlags <$>
       strArgument (mconcat [metavar "REPORT", help "The preconfigured report to run"]))

parseCommandWork :: ParserInfo Command
parseCommandWork = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the work overview"
    parser =
      CommandWork <$>
      (WorkFlags <$> parseContextNameArg <*> parseTimeFilterArg <*> parseFilterArgs <*>
       parseProjectionArgs <*>
       parseSorterArgs <*>
       parseHideArchiveFlag)

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the \"waiting\" tasks"
    parser =
      CommandWaiting <$>
      (WaitingFlags <$> parseFilterArgs <*> parseHideArchiveFlag <*> parseThresholdFlag)

parseThresholdFlag :: Parser (Maybe Word)
parseThresholdFlag =
  option
    (Just <$> auto)
    (mconcat [long "threshold", value Nothing, help "The threshold at which to color entries red"])

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the next actions"
    parser = CommandNext <$> (NextFlags <$> parseFilterArgs <*> parseHideArchiveFlag)

parseCommandClock :: ParserInfo Command
parseCommandClock = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the clock table"
    parser =
      CommandClock <$>
      (ClockFlags <$> parseFilterArgs <*> parsePeriod <*> parseTimeBlock <*> parseOutputFormat <*>
       parseClockFormatFlags <*>
       parseClockReportStyle <*>
       parseHideArchiveFlag)

parseClockFormatFlags :: Parser (Maybe ClockFormatFlags)
parseClockFormatFlags =
  Just <$>
  ((flag' ClockFormatTemporalFlag (long "temporal") <*> parseTemporalClockResolution) <|>
   flag' ClockFormatDecimalFlag (long "decimal") <*> parseDecimalClockResolution) <|>
  pure Nothing

parseTemporalClockResolution :: Parser (Maybe TemporalClockResolution)
parseTemporalClockResolution =
  Just <$>
  (flag' TemporalSecondsResolution (long "seconds-resolution") <|>
   flag' TemporalMinutesResolution (long "minutes-resolution") <|>
   flag' TemporalHoursResolution (long "hours-resolution")) <|>
  pure Nothing

parseDecimalClockResolution :: Parser (Maybe DecimalClockResolution)
parseDecimalClockResolution =
  Just <$>
  (flag' DecimalQuarterResolution (long "quarters-resolution") <|>
   (flag' DecimalResolution (long "resolution") <*> argument auto (help "significant digits")) <|>
   flag' DecimalHoursResolution (long "hours-resolution")) <|>
  pure Nothing

parseClockReportStyle :: Parser (Maybe ClockReportStyle)
parseClockReportStyle =
  Just <$> (flag' ClockForest (long "forest") <|> flag' ClockFlat (long "flat")) <|> pure Nothing

parseCommandAgenda :: ParserInfo Command
parseCommandAgenda = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the agenda"
    parser =
      CommandAgenda <$>
      (AgendaFlags <$> parseFilterArgs <*> parseHistoricityFlag <*> parseTimeBlock <*>
       parseHideArchiveFlag)

parseCommandProjects :: ParserInfo Command
parseCommandProjects = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the projects overview"
    parser = CommandProjects <$> (ProjectsFlags <$> parseProjectFilterArgs)

parseCommandStuck :: ParserInfo Command
parseCommandStuck = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the stuck projects overview"
    parser = CommandStuck <$> (StuckFlags <$> parseProjectFilterArgs)

parseCommandLog :: ParserInfo Command
parseCommandLog = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print a log of what has happened."
    parser =
      CommandLog <$>
      (LogFlags <$> parseFilterArgs <*> parsePeriod <*> parseTimeBlock <*> parseHideArchiveFlag)

parseCommandStats :: ParserInfo Command
parseCommandStats = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the stats actions and warn if a file does not have one."
    parser = CommandStats <$> (StatsFlags <$> parsePeriod)

parseCommandTags :: ParserInfo Command
parseCommandTags = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print all the tags that are in use"
    parser = CommandTags <$> (TagsFlags <$> parseFilterArgs)

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseFlags

parseHistoricityFlag :: Parser (Maybe AgendaHistoricity)
parseHistoricityFlag =
  Just <$> (flag' HistoricalAgenda (long "historical") <|> flag' FutureAgenda (long "future")) <|>
  pure Nothing

parseHideArchiveFlag :: Parser (Maybe HideArchive)
parseHideArchiveFlag =
  (Just <$>
   (flag' HideArchive (mconcat [long "hide-archived", help "ignore archived files."]) <|>
    flag'
      Don'tHideArchive
      (mconcat [short 'a', long "show-archived", help "Don't ignore archived files."]))) <|>
  pure Nothing

parseContextNameArg :: Parser ContextName
parseContextNameArg =
  argument (ContextName <$> str) (mconcat [metavar "CONTEXT", help "The context that you are in"])

parseTimeFilterArg :: Parser (Filter Time)
parseTimeFilterArg =
  argument
    (eitherReader (fmap (FilterOrd LEC) . parseTime . T.pack))
    (mconcat [metavar "TIME_FILTER", help "A filter to filter by time"])

parseFilterArgs :: Parser (Maybe EntryFilter)
parseFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty <$>
  many
    (argument
       (eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
       (mconcat [metavar "FILTER", help "A filter to filter entries by"]))

parseFilterArg :: Parser (Maybe EntryFilter)
parseFilterArg =
  argument
    (Just <$> eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
    (mconcat [value Nothing, metavar "FILTER", help "A filter to filter entries by"])

parseProjectFilterArgs :: Parser (Maybe ProjectFilter)
parseProjectFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty <$>
  many
    (argument
       (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
       (mconcat [metavar "FILTER", help "A filter to filter projects by"]))

parseProjectionArgs :: Parser (Maybe (NonEmpty Projection))
parseProjectionArgs =
  NE.nonEmpty . catMaybes <$>
  many
    (option
       (Just <$> maybeReader (parseProjection . T.pack))
       (mconcat
          [ long "add-column"
          , long "project"
          , metavar "PROJECTION"
          , help "A projection to project entries onto fields"
          ]))

parseSorterArgs :: Parser (Maybe Sorter)
parseSorterArgs =
  fmap (foldl1 AndThen) . NE.nonEmpty . catMaybes <$>
  many
    (option
       (Just <$> maybeReader (parseSorter . T.pack))
       (mconcat [long "sort", metavar "SORTER", help "A sorter to sort entries by"]))

parseTimeBlock :: Parser (Maybe TimeBlock)
parseTimeBlock =
  Just <$>
  choices
    [ flag' DayBlock $ mconcat [long "day-block", help "blocks of one day"]
    , flag' WeekBlock $ mconcat [long "week-block", help "blocks of one week"]
    , flag' MonthBlock $ mconcat [long "month-block", help "blocks of one month"]
    , flag' YearBlock $ mconcat [long "year-block", help "blocks of one year"]
    , flag' OneBlock $ mconcat [long "one-block", help "a single block"]
    ] <|>
  pure Nothing

parsePeriod :: Parser (Maybe Period)
parsePeriod =
  Just <$>
  (parseBeginEnd <|>
   choices
     [ flag' Today (mconcat [long "today", help "today"])
     , flag' Yesterday (mconcat [long "yesterday", help "yesterday"])
     , flag' ThisWeek (mconcat [long "this-week", help "this week"])
     , flag' LastWeek (mconcat [long "last-week", help "last week"])
     , flag' ThisMonth (mconcat [long "this-month", help "this month"])
     , flag' LastMonth (mconcat [long "last-month", help "last month"])
     , flag' ThisYear (mconcat [long "this-year", help "this year"])
     , flag' LastYear (mconcat [long "last-year", help "last year"])
     , flag' AllTime (mconcat [long "all-time", help "all time"])
     ]) <|>
  pure Nothing
  where
    parseBeginEnd :: Parser Period
    parseBeginEnd =
      (\mb me ->
         case (mb, me) of
           (Nothing, Nothing) -> AllTime
           (Just begin, Nothing) -> BeginOnly begin
           (Nothing, Just end) -> EndOnly end
           (Just begin, Just end) -> BeginEnd begin end) <$>
      option
        (Just <$> maybeReader parseLocalBegin)
        (mconcat [value Nothing, long "begin", metavar "LOCALTIME", help "start time (inclusive)"]) <*>
      option
        (Just <$> maybeReader parseLocalEnd)
        (mconcat [value Nothing, long "end", metavar "LOCALTIME", help "end tiem (inclusive)"])
    parseLocalBegin :: String -> Maybe LocalTime
    parseLocalBegin s = LocalTime <$> parseLocalDay s <*> pure midnight <|> parseExactly s
    parseLocalEnd :: String -> Maybe LocalTime
    parseLocalEnd s =
      (LocalTime <$> (addDays 1 <$> parseLocalDay s) <*> pure midnight) <|> parseExactly s
    parseExactly :: String -> Maybe LocalTime
    parseExactly s =
      parseTimeM True defaultTimeLocale "%F %R" s <|> parseTimeM True defaultTimeLocale "%F %T" s
    parseLocalDay :: String -> Maybe Day
    parseLocalDay = parseTimeM True defaultTimeLocale "%F"

parseOutputFormat :: Parser (Maybe OutputFormat)
parseOutputFormat =
  Just <$>
  choices
    [ flag' OutputPretty $ mconcat [long "pretty", help "pretty text"]
    , flag' OutputYaml $ mconcat [long "yaml", help "Yaml"]
    , flag' OutputJSON $ mconcat [long "json", help "single-line JSON"]
    , flag' OutputJSONPretty $ mconcat [long "pretty-json", help "pretty JSON"]
    ] <|>
  pure Nothing

choices :: [Parser a] -> Parser a
choices = asum
