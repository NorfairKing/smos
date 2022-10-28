{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.OptParse
  ( module Smos.Calendar.Import.OptParse,
    module Smos.Calendar.Import.OptParse.Types,
  )
where

import Control.Monad
import qualified Data.ByteString as SB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import qualified Env
import Network.URI
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Path.IO
import Paths_smos_calendar_import
import Smos.Calendar.Import.OptParse.Types
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified System.Environment as System

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
  setSources <- fmap catMaybes $
    forM (maybe [] calendarImportConfSources (mConf >>= confCalendarImportConfiguration)) $ \SourceConfiguration {..} -> do
      mOriginURIString <- case sourceConfOrigin of
        Just uri -> pure $ Just uri
        Nothing -> case sourceConfOriginFile of
          Just uriFile -> Just . T.unpack . T.strip . TE.decodeUtf8 <$> SB.readFile uriFile
          Nothing -> pure Nothing
      case mOriginURIString of
        Nothing -> pure Nothing
        Just originURIString -> do
          let sourceName = sourceConfName
          sourceDestinationFile <- parseRelFile sourceConfDestinationFile
          sourceOrigin <- case parseURI originURIString of
            Just uri -> pure $ WebOrigin uri
            Nothing -> do
              putStrLn $ "Couldn't parse into an URI, assuming it's a file: " <> originURIString
              FileOrigin <$> resolveFile' originURIString
          pure (Just Source {..})
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
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Calendar Import Tool version: " <> showVersion version,
            ""
          ]
            ++ writeDataVersionsHelpMessage

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
      <*> optional (Env.var Env.auto "DEBUG" (Env.help "Whether to output debug info"))
