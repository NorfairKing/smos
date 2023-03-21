{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Notify.OptParse
  ( module Smos.Notify.OptParse,
    module Smos.Notify.OptParse.Types,
  )
where

import Control.Arrow (left)
import Control.Monad.Logger
import Data.Maybe
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Path.IO
import Paths_smos_notify
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Directory.Config
import Smos.Directory.OptParse
import Smos.Notify.OptParse.Types
import qualified System.Environment as System
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  deriveSettings (flagWithRestFlags flags) (envWithRestEnv env) config

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mConf = do
  let mc :: (NotifyConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= confNotifyConfiguration >>= func
  setDirectorySettings <-
    combineToDirectoryConfig
      defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mConf)
  setDatabase <- case flagDatabase <|> envDatabase <|> mc notifyConfDatabase of
    Just fp -> resolveFile' fp
    Nothing -> defaultDatabaseFile
  setNotifySend <- case flagNotifySend <|> envNotifySend <|> mc notifyConfNotifySend of
    Just fp -> resolveFile' fp
    Nothing -> do
      me <- findExecutable [relfile|notify-send|]
      case me of
        Nothing -> die "could not find a notify-send executable."
        Just e -> pure e
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc notifyConfLogLevel
  pure Settings {..}

smosRelDir :: Path Rel Dir
smosRelDir = [reldir|smos|]

defaultDatabaseFile :: IO (Path Abs File)
defaultDatabaseFile = do
  dataDir <- getXdgDir XdgData (Just smosRelDir)
  resolveFile dataDir "notify.sqlite3"

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
            "Smos Notification Tool version: " <> showVersion version,
            ""
          ]
            ++ readDataVersionsHelpMessage

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> parseDirectoryFlags
    <*> optional
      ( strOption
          ( mconcat
              [ metavar "FILEPATH",
                long "database",
                help "The path to store the notification database at"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ metavar "FILEPATH",
                long "notify-send",
                help "The path to the notify-send executable"
              ]
          )
      )
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

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> directoryEnvironmentParser
      <*> optional (Env.var Env.str "SESSION_PATH" (Env.help "The path to store the notification database at"))
      <*> optional (Env.var Env.str "NOTIFY_SEND" (Env.help "The path to the notify-send executable"))
      <*> optional (Env.var logLevelReader "LOG_LEVEL" (Env.help "log level"))
  where
    logLevelReader = left Env.UnreadError . parseLogLevel
