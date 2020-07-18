{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.OptParse
  ( module Smos.Calendar.Import.OptParse,
    module Smos.Calendar.Import.OptParse.Types,
  )
where

import Control.Monad
import Data.Maybe
import qualified Env
import Network.URI
import Options.Applicative
import Path
import Path.IO
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
  setSources <- case mc calendarImportConfSources of
    Nothing -> die "No sources configured."
    Just ss -> forM ss $ \SourceConfiguration {..} -> do
      let sourceName = sourceConfName
      sourceDestinationFile <- parseRelFile sourceConfDestinationFile
      sourceOrigin <- case parseURI sourceConfOrigin of
        Just uri -> pure $ WebOrigin uri
        Nothing -> do
          putStrLn $ "Couldn't parse into an URI, assuming it's a file: " <> sourceConfOrigin
          FileOrigin <$> resolveFile' sourceConfOrigin
      pure Source {..}
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
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo (Report.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> Report.parseFlagsWithConfigFile parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-archive"

parseFlags :: Parser Flags
parseFlags =
  Flags <$> Report.parseDirectoryFlags
    <*> (Just <$> switch (mconcat [long "debug", help "Turn on debug output"]))

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment <$> Report.directoryEnvironmentParser
      <*> Env.var (fmap Just . Env.auto) "DEBUG" (mE <> Env.help "Whether to output debug info")
  where
    mE = Env.def Nothing <> Env.keep
