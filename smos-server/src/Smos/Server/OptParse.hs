{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse
  ( module Smos.Server.OptParse,
    module Smos.Server.OptParse.Types,
  )
where

import Control.Arrow
import Control.Monad.Logger
import Data.Maybe
import Data.SemVer as Version (toString)
import qualified Data.Text as T
import Data.Time
import Data.Version
import qualified Env
import Looper
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_server
import Smos.API
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
          serveSetSigningKeyFile <-
            case serveFlagSigningKeyFile <|> envSigningKeyFile <|> (mc >>= confSigningKeyFile) of
              Nothing -> resolveFile' "smos-signing-key.json"
              Just fp -> resolveFile' fp
          let serveSetMaxBackupsPerUser = serveFlagMaxBackupsPerUser <|> envMaxBackupsPerUser <|> (mc >>= confMaxBackupsPerUser)
          let serveSetMaxBackupSizePerUser = serveFlagMaxBackupSizePerUser <|> envMaxBackupSizePerUser <|> (mc >>= confMaxBackupSizePerUser)
          let serveSetBackupInterval = fromMaybe nominalDay $ serveFlagBackupInterval <|> envBackupInterval <|> (mc >>= confBackupInterval)
          let serveSetAutoBackupLooperSettings =
                deriveLooperSettings
                  0
                  (hours 1)
                  serveFlagAutoBackupLooperFlags
                  envAutoBackupLooperEnv
                  (mc >>= confAutoBackupLooperConfiguration)
          let serveSetBackupGarbageCollectionLooperSettings =
                deriveLooperSettings
                  (minutes 1)
                  (hours 24)
                  serveFlagBackupGarbageCollectionLooperFlags
                  envBackupGarbageCollectionLooperEnv
                  (mc >>= confBackupGarbageCollectionLooperConfiguration)
          let serveSetFileMigrationLooperSettings =
                deriveLooperSettings
                  (minutes 2)
                  (hours 24)
                  serveFlagFileMigrationLooperFlags
                  envFileMigrationLooperEnv
                  (mc >>= confFileMigrationLooperConfiguration)
          let serveSetAdmin = serveFlagAdmin <|> envAdmin <|> (mc >>= confAdmin)
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
      <*> Env.var (fmap Just . Env.str) "SIGNING_KEY_FILE" (mE <> Env.help "The file to store the JWT signing key in")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "The port to serve web requests on")
      <*> Env.var (fmap Just . Env.auto) "MAX_BACKUPS_PER_USER" (mE <> Env.help "The maximum number of backups per user")
      <*> Env.var (fmap Just . Env.auto) "MAX_BACKUP_SIZE_PER_USER" (mE <> Env.help "The maximum number of bytes that backups can take up per user")
      <*> Env.var (fmap (Just . (fromIntegral :: Int -> NominalDiffTime)) . Env.auto) "BACKUP_INTERVAL" (mE <> Env.help "The interval between automatic backups (seconds)")
      <*> looperEnvironmentParser "AUTO_BACKUP"
      <*> looperEnvironmentParser "BACKUP_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "FILE_MIGRATOR"
      <*> Env.var (fmap Just . (left Env.UnreadError . parseUsernameWithError . T.pack)) "ADMIN" (mE <> Env.help "The user that will have admin rights")
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
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map
          Doc.text
          [ "",
            "Smos API Server version: " <> showVersion version,
            "",
            "Server API version: " <> Version.toString apiVersion
          ]

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
    <$> optional
      ( option
          (maybeReader parseLogLevel)
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
      ( strOption
          ( mconcat
              [ long "uuid-file",
                help "The file to use for the server uuid",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database-file",
                help "The file to use for the server database",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "signing-key-file",
                help "The file to use for the JWT signing key database",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                help "The port to serve on",
                metavar "PORT"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-backup-per-user",
                metavar "NUMBER",
                help "The maximum number of backups per user"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-backup-size-per-user",
                metavar "BYTES",
                help "The maximum number of bytes that backups can take up per user"
              ]
          )
      )
    <*> optional
      ( option
          ((fromIntegral :: Int -> NominalDiffTime) <$> auto)
          ( mconcat
              [ long "backup-interval",
                metavar "SECONDS",
                help "The interval between automatic backups"
              ]
          )
      )
    <*> getLooperFlags "auto-backup"
    <*> getLooperFlags "backup-garbage-collector"
    <*> getLooperFlags "file-migrator"
    <*> optional
      ( option
          (eitherReader $ parseUsernameWithError . T.pack)
          ( mconcat
              [ long "admin",
                metavar "USERNAME",
                help "The user that will have admin rights"
              ]
          )
      )

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", help "The config file to use", metavar "FILEPATH", value Nothing])
