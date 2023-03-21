{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.OptParse
  ( module Smos.Calendar.Import.OptParse,
    module Smos.Calendar.Import.OptParse.Types,
  )
where

import Control.Arrow (left)
import Control.Monad
import Control.Monad.Logger
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
import Smos.CLI.OptParse as CLI
import Smos.Calendar.Import.OptParse.Types
import Smos.Data
import Smos.Directory.OptParse
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings (flagWithRestFlags flags) (envWithRestEnv env) config

getConfig :: FlagsWithConfigFile Flags -> EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfig = getConfiguration

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mConf = do
  let mc :: (CalendarImportConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= confCalendarImportConfiguration >>= func
  setDirectorySettings <-
    combineToDirectorySettings
      defaultDirectorySettings
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
  let setLogLevel =
        fromMaybe (if setDebug then LevelDebug else LevelInfo) $
          flagLogLevel <|> envLogLevel <|> mc calendarImportConfLogLevel
  pure Settings {..}

getFlags :: IO (FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult (FlagsWithConfigFile Flags)
runArgumentsParser = CLI.execOptionParserPure flagsParser

flagsParser :: ParserInfo (FlagsWithConfigFile Flags)
flagsParser = info (helper <*> parseFlagsWithConfigFile parseFlags) help_
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
  Flags
    <$> parseDirectoryFlags
    <*> optional
      ( option
          (eitherReader parseLogLevel)
          ( mconcat
              [ long "log-level",
                help $
                  unwords
                    [ "The log level to use, options:",
                      show $ map renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
                    ]
              ]
          )
      )
    <*> optional
      ( switch
          ( mconcat
              [ long "debug",
                help "Turn on debug output"
              ]
          )
      )

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> directoryEnvironmentParser
      <*> optional (Env.var (left Env.UnreadError . parseLogLevel) "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
      <*> optional (Env.var Env.auto "DEBUG" (Env.help "Whether to output debug info"))
