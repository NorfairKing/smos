{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.OptParse
  ( module Smos.Calendar.Import.OptParse,
    module Smos.Calendar.Import.OptParse.Types,
  )
where

import Data.List.NonEmpty as NE
import Data.Maybe
import qualified Env
import Options.Applicative
import Path
import Smos.Calendar.Import.OptParse.Types
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified System.Environment as System
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

getConfig :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfig = Report.getConfiguration

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mConf = do
  let mc :: (CalendarImportConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= confCalendarImportConfiguration >>= func
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mConf)
  setDestinationFile <- parseRelFile $ fromMaybe "calendar.smos" $ flagDestinationFile <|> envDestinationFile <|> mc calendarImportConfDestinationFile
  setSources <- case flagSources <|> ((:| []) <$> envSource) <|> mc calendarImportConfSources of
    Nothing -> die "No sources configured."
    Just ss -> pure ss
  let setDebug = fromMaybe False $ flagDebug <|> envDebug <|> mc calendarImportConfDebug
  pure Settings {..}

getFlags :: IO (Report.FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult (Report.FlagsWithConfigFile Flags)
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

flagsParser :: ParserInfo (Report.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> Report.parseFlagsWithConfigFile parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-archive"

parseFlags :: Parser Flags
parseFlags =
  Flags <$> Report.parseDirectoryFlags
    <*> destinationOption
    <*> sourcesOptions
    <*> (Just <$> switch (mconcat [long "debug", help "Turn on debug output"]))

destinationOption :: Parser (Maybe FilePath)
destinationOption =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The destination path within the workflow directory",
          long "destination",
          value Nothing
        ]
    )

sourcesOptions :: Parser (Maybe (NonEmpty String))
sourcesOptions =
  NE.nonEmpty
    <$> many
      ( option
          str
          ( mconcat
              [ metavar "URL",
                help "A source url to import from",
                long "source"
              ]
          )
      )

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment <$> Report.directoryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "DESTINATION" (mE <> Env.help "The destination path within the workflow directory")
      <*> Env.var (fmap Just . Env.str) "SOURCE" (mE <> Env.help "A source url to import the calendar from. Use flags or a config file to import from multiple sources.")
      <*> Env.var (fmap Just . Env.auto) "DEBUG" (mE <> Env.help "Whether to output debug info")
  where
    mE = Env.def Nothing <> Env.keep
