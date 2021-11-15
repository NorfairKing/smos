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
import Smos.Data
import Smos.Notify.OptParse.Types
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
  let mc :: (NotifyConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= confNotifyConfiguration >>= func
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mConf)
  setDatabase <- case flagDatabase <|> envDatabase <|> mc notifyConfDatabase of
    Nothing -> defaultDatabaseFile
    Just fp -> resolveFile' fp
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc notifyConfLogLevel
  pure Settings {..}

smosRelDir :: Path Rel Dir
smosRelDir = [reldir|smos|]

defaultDatabaseFile :: IO (Path Abs File)
defaultDatabaseFile = do
  dataDir <- getXdgDir XdgData (Just smosRelDir)
  resolveFile dataDir "notify.sqlite3"

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
            "Smos Notification Tool version: " <> showVersion version,
            ""
          ]
            ++ readDataVersionsHelpMessage

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> Report.parseDirectoryFlags
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

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment
      <$> Report.directoryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "SESSION_PATH" (mE <> Env.help "The path to store the notification database at")
      <*> Env.var (fmap Just . logLevelReader) "LOG_LEVEL" (mE <> Env.help "log level")
  where
    mE = Env.def Nothing <> Env.keep
    logLevelReader = left Env.UnreadError . parseLogLevel
