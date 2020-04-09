{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse
  ( getInstructions
  , Instructions(..)
  , Dispatch(..)
  , ServeSettings(..)
  , Settings(..)
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Logger
import Data.Maybe
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)
import Path
import Path.IO
import Text.Read (readMaybe)

import qualified System.Environment as System
import System.Exit

import Options.Applicative

import Smos.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments c Flags {..}) Environment {..} mc =
  Instructions <$> getDispatch <*> getSettings
  where
    getDispatch =
      case c of
        CommandServe ServeFlags {..} -> do
          let serveSetLogLevel =
                fromMaybe LevelWarn $ serveFlagLogLevel <|> envLogLevel <|> (mc >>= confLogLevel)
          let serveSetPort = fromMaybe 8000 $ serveFlagPort <|> envPort <|> (mc >>= confPort)
          serveSetUUIDFile <-
            case serveFlagUUIDFile <|> envUUIDFile <|> (mc >>= confUUIDFile) of
              Nothing -> resolveFile' "smos-server-uuid.json"
              Just fp -> resolveFile' fp
          serveSetDatabaseFile <-
            case serveFlagDatabaseFile <|> envDatabaseFile <|> (mc >>= confDatabaseFile) of
              Nothing -> resolveFile' "smos-server-database.sqlite3"
              Just fp -> resolveFile' fp
          pure $ DispatchServe ServeSettings {..}
    getSettings = pure Settings

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let getEnv :: String -> Maybe String
      getEnv key = ("SMOS_SERVER_" ++ key) `lookup` env
      readEnv :: Read a => String -> Maybe a
      readEnv key = getEnv key >>= readMaybe
  envLogLevel <-
    forM (getEnv "LOG_LEVEL") $ \s ->
      case parseLogLevel s of
        Nothing -> fail $ "Unknown log level: " <> s
        Just ll -> pure ll
  let envConfigFile = getEnv "CONFIGURATION_FILE" <|> getEnv "CONFIG_FILE" <|> getEnv "CONFIG"
      envPort = readEnv "PORT"
      envUUIDFile = getEnv "UUID_FILE" <|> getEnv "UUID"
      envDatabaseFile = getEnv "DATABASE_FILE" <|> getEnv "DATABASE"
  pure Environment {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  mConfigFile <- forM (msum [flagConfigFile, envConfigFile]) $ \fp -> resolveFile' fp
  forM mConfigFile $ \configFile -> do
    errOrConfig <-
      fmap (left prettyPrintParseException) $ Yaml.decodeFileEither $ fromAbsFile configFile
    case errOrConfig of
      Left err -> die err
      Right conf -> pure conf

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
    description = "smos-server"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    modifier = fullDesc <> progDesc "Serve as the sync server"
    parser =
      CommandServe <$>
      (ServeFlags <$>
       option
         (Just <$> maybeReader parseLogLevel)
         (mconcat
            [ long "log-level"
            , help $
              unwords
                [ "The log level to use, options:"
                , show $ map renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
                ]
            , value Nothing
            ]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "uuid-file"
            , help "The file to use for the server uuid"
            , metavar "FILEPATH"
            , value Nothing
            ]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "database-file"
            , help "The file to use for the server database"
            , metavar "FILEPATH"
            , value Nothing
            ]) <*>
       option
         (Just <$> auto)
         (mconcat [long "port", help "The port to serve on", metavar "PORT", value Nothing]))

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  option
    (Just <$> str)
    (mconcat [long "config-file", help "The config file to use", metavar "FILEPATH", value Nothing])
