{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse
  ( module Smos.Server.OptParse,
    module Smos.Server.OptParse.Types,
  )
where

import Control.Monad.Logger
import Data.Maybe
import qualified Env
import Options.Applicative
import Path.IO
import Smos.Server.OptParse.Types
import qualified System.Environment as System
import YamlParse.Applicative (readConfigFile)

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
getEnvironment = Env.parse (Env.header "Enviromnent") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SMOS_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . (maybe (Left $ Env.UnreadError "Unknown log level") Right . parseLogLevel)) "LOG_LEVEL" (mE <> Env.help "The minimal severity of log messages")
      <*> Env.var (fmap Just . Env.str) "UUID_FILE" (mE <> Env.help "The file to store the server uuid in")
      <*> Env.var (fmap Just . Env.str) "DATABASE_FILE" (mE <> Env.help "The file to store the server database in")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "The port to serve web requests on")
  where
    mE = Env.def Nothing <> Env.keep

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readConfigFile

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
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
      CommandServe <$> parseServeFlags

parseServeFlags :: Parser ServeFlags
parseServeFlags =
  ServeFlags
    <$> option
      (Just <$> maybeReader parseLogLevel)
      ( mconcat
          [ long "api-log-level",
            help $
              unwords
                [ "The log level to use, options:",
                  show $ map renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
                ],
            value Nothing
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "uuid-file",
            help "The file to use for the server uuid",
            metavar "FILEPATH",
            value Nothing
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "database-file",
            help "The file to use for the server database",
            metavar "FILEPATH",
            value Nothing
          ]
      )
    <*> option
      (Just <$> auto)
      (mconcat [long "api-port", help "The port to serve on", metavar "PORT", value Nothing])

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", help "The config file to use", metavar "FILEPATH", value Nothing])
