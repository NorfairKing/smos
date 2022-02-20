{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Report.OptParse
  ( module Smos.Report.OptParse,
    module Smos.Report.OptParse.Types,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Aeson (FromJSON)
import Data.Aeson as JSON (eitherDecodeFileStrict)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time hiding (parseTime)
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)
import qualified Env
import Options.Applicative
import Path
import Path.IO
import Smos.Report.Agenda.Types
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.OptParse.Types
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

combineToConfig ::
  SmosReportConfig -> Flags -> Environment -> Maybe Configuration -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
  smosReportConfigDirectoryConfig <- combineToDirectoryConfig (smosReportConfigDirectoryConfig src) flagDirectoryFlags envDirectoryEnvironment (confDirectoryConf <$> mc)
  smosReportConfigWaitingConfig <- combineToWaitingReportConfig (smosReportConfigWaitingConfig src) (mc >>= confWaitingReportConf)
  smosReportConfigStuckConfig <- combineToStuckReportConfig (smosReportConfigStuckConfig src) (mc >>= confStuckReportConf)
  smosReportConfigWorkConfig <- combineToWorkReportConfig (smosReportConfigWorkConfig src) (mc >>= confWorkReportConf)
  pure $ SmosReportConfig {..}

combineToDirectoryConfig :: DirectoryConfig -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectoryConfiguration -> IO DirectoryConfig
combineToDirectoryConfig dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    case msum [dirFlagWorkflowDir, dirEnvWorkflowDir, mc >>= (fmap T.unpack . directoryConfWorkflowDir)] of
      Nothing -> pure $ directoryConfigWorkflowFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ AbsoluteWorkflow ad
  afs <-
    case msum [dirFlagArchiveDir, dirEnvArchiveDir, mc >>= (fmap T.unpack . directoryConfArchiveDir)] of
      Nothing -> pure $ directoryConfigArchiveFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pfs <-
    case msum [dirFlagProjectsDir, dirEnvProjectsDir, mc >>= (fmap T.unpack . directoryConfProjectsDir)] of
      Nothing -> pure $ directoryConfigProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ProjectsAbsolute ad
  apfs <-
    case msum
      [ dirFlagArchivedProjectsDir,
        dirEnvArchivedProjectsDir,
        mc >>= (fmap T.unpack . directoryConfArchivedProjectsDir)
      ] of
      Nothing -> pure $ directoryConfigArchivedProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchivedProjectsAbsolute ad
  pure $
    dc
      { directoryConfigWorkflowFileSpec = wfs,
        directoryConfigArchiveFileSpec = afs,
        directoryConfigProjectsFileSpec = pfs,
        directoryConfigArchivedProjectsFileSpec = apfs
      }

combineToWaitingReportConfig :: WaitingReportConfig -> Maybe WaitingReportConfiguration -> IO WaitingReportConfig
combineToWaitingReportConfig wrc mc = do
  let WaitingReportConfig _ = undefined
  pure $
    wrc
      { waitingReportConfigThreshold = fromMaybe defaultWaitingThreshold $ mc >>= waitingReportConfThreshold
      }

combineToStuckReportConfig :: StuckReportConfig -> Maybe StuckReportConfiguration -> IO StuckReportConfig
combineToStuckReportConfig wrc mc = do
  let StuckReportConfig _ = undefined
  pure $
    wrc
      { stuckReportConfigThreshold = fromMaybe defaultStuckThreshold $ mc >>= stuckReportConfThreshold
      }

combineToWorkReportConfig :: WorkReportConfig -> Maybe WorkReportConfiguration -> IO WorkReportConfig
combineToWorkReportConfig wrc mc = do
  let WorkReportConfig _ _ _ _ _ _ = undefined
  pure $
    wrc
      { workReportConfigBaseFilter =
          (mc >>= workReportConfBaseFilter) <|> workReportConfigBaseFilter wrc,
        workReportConfigChecks = fromMaybe (workReportConfigChecks wrc) (mc >>= workReportConfChecks),
        workReportConfigContexts = fromMaybe (workReportConfigContexts wrc) (mc >>= workReportConfContexts),
        workReportConfigTimeProperty = mc >>= workReportConfTimeFilterProperty,
        workReportConfigProjection = fromMaybe defaultProjection (mc >>= workReportConfProjection),
        workReportConfigSorter = mc >>= workReportConfSorter
      }

parseFlags :: Parser Flags
parseFlags =
  Flags <$> parseDirectoryFlags

parseFlagsWithConfigFile :: Parser a -> Parser (FlagsWithConfigFile a)
parseFlagsWithConfigFile p =
  FlagsWithConfigFile <$> parseConfigFileFlag <*> p

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILEPATH",
              help "The config file to use",
              long "config-file",
              completer $ bashCompleter "file"
            ]
        )
    )

parseDirectoryFlags :: Parser DirectoryFlags
parseDirectoryFlags =
  DirectoryFlags <$> parseWorkflowDirFlag <*> parseArchiveDirFlag
    <*> parseProjectsDirFlag
    <*> parseArchivedProjectsDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILEPATH",
              help "The workflow directory to use",
              long "workflow-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILEPATH",
              help "The archive directory to use",
              long "archive-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseProjectsDirFlag :: Parser (Maybe FilePath)
parseProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILEPATH",
              help "The projects directory to use",
              long "projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

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

parseArchivedProjectsDirFlag :: Parser (Maybe FilePath)
parseArchivedProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "FILEPATH",
              help "The archived projects directory to use",
              long "archived-projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseProjectFilterArgs :: Parser (Maybe ProjectFilter)
parseProjectFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to filter projects by"])
      )

parseFileFilterArgs :: Parser (Maybe (Filter (Path Rel File)))
parseFileFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to smos files by"])
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
            flag' ThisWeek (mconcat [long "this-week", help "this week"]),
            flag' NextWeek (mconcat [long "next-week", help "next week"]),
            flag' LastMonth (mconcat [long "last-month", help "last month"]),
            flag' ThisMonth (mconcat [long "this-month", help "this month"]),
            flag' NextMonth (mconcat [long "next-month", help "next month"]),
            flag' LastYear (mconcat [long "last-year", help "last year"]),
            flag' ThisYear (mconcat [long "this-year", help "this year"]),
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
              (maybeReader parseLocalBegin)
              (mconcat [long "begin", metavar "LOCALTIME", help "start time (inclusive)"])
          )
        <*> optional
          ( option
              (maybeReader parseLocalEnd)
              (mconcat [long "end", metavar "LOCALTIME", help "end time (inclusive)"])
          )
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

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> directoryEnvironmentParser

directoryEnvironmentParser :: Env.Parser Env.Error DirectoryEnvironment
directoryEnvironmentParser =
  DirectoryEnvironment
    <$> optional (Env.var Env.str "WORKFLOW_DIR" (Env.help "Workflow directory"))
    <*> optional (Env.var Env.str "ARCHIVE_DIR" (Env.help "Archive directory"))
    <*> optional (Env.var Env.str "PROJECTS_DIR" (Env.help "Projects directory"))
    <*> optional (Env.var Env.str "ARCHIVED_PROJECTS_DIR" (Env.help "Archived projects directory"))

envWithConfigFileParser :: Env.Parser Env.Error a -> Env.Parser Env.Error (EnvWithConfigFile a)
envWithConfigFileParser p =
  EnvWithConfigFile
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Workflow directory"))
    <*> p

defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles = do
  home <- getHomeDir
  homeConfigDir <- resolveDir home ".smos"
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|smos|])
  let inDirs = do
        d <- [xdgConfigDir, homeConfigDir]
        pure $ d </> [relfile|config|]
  plainFile <- resolveFile home ".smos"
  let files = inDirs ++ [plainFile]
  pure $ mapMaybe (replaceExtension ".yaml") files

parseYamlConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseYamlConfig configFile =
  fmap (left prettyPrintParseException) $ decodeFileEither $ fromAbsFile configFile

parseJSONConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseJSONConfig configFile = JSON.eitherDecodeFileStrict $ fromAbsFile configFile

getConfiguration :: HasCodec a => FlagsWithConfigFile b -> EnvWithConfigFile c -> IO (Maybe a)
getConfiguration FlagsWithConfigFile {..} EnvWithConfigFile {..} = do
  case flagWithConfigFile <|> envWithConfigFile of
    Just sf -> resolveFile' sf >>= readYamlConfigFile
    Nothing -> defaultConfigFiles >>= readFirstYamlConfigFile
