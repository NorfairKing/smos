{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse
  ( module Smos.Server.OptParse,
    module Smos.Server.OptParse.Types,
  )
where

import Autodocodec.Yaml
import Control.Arrow
import Control.Monad.Logger
import Data.Maybe
import Data.SemVer as Version (toString)
import qualified Data.Set as S
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

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mc = do
  let settingLogLevel =
        fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> (mc >>= confLogLevel)
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> (mc >>= confPort)
  settingUUIDFile <-
    case flagUUIDFile <|> envUUIDFile <|> (mc >>= confUUIDFile) of
      Nothing -> resolveFile' "smos-server-uuid.json"
      Just fp -> resolveFile' fp
  settingDatabaseFile <-
    case flagDatabaseFile <|> envDatabaseFile <|> (mc >>= confDatabaseFile) of
      Nothing -> resolveFile' "smos-server-database.sqlite3"
      Just fp -> resolveFile' fp
  settingSigningKeyFile <-
    case flagSigningKeyFile <|> envSigningKeyFile <|> (mc >>= confSigningKeyFile) of
      Nothing -> resolveFile' "smos-signing-key.json"
      Just fp -> resolveFile' fp
  let settingMaxBackupsPerUser = flagMaxBackupsPerUser <|> envMaxBackupsPerUser <|> (mc >>= confMaxBackupsPerUser)
  let settingMaxBackupSizePerUser = flagMaxBackupSizePerUser <|> envMaxBackupSizePerUser <|> (mc >>= confMaxBackupSizePerUser)
  let settingBackupInterval = fromMaybe nominalDay $ flagBackupInterval <|> envBackupInterval <|> (mc >>= confBackupInterval)
  let settingAutoBackupLooperSettings =
        deriveLooperSettings
          0
          (hours 1)
          flagAutoBackupLooperFlags
          envAutoBackupLooperEnv
          (mc >>= confAutoBackupLooperConfiguration)
  let settingBackupGarbageCollectionLooperSettings =
        deriveLooperSettings
          (minutes 1)
          (hours 24)
          flagBackupGarbageCollectionLooperFlags
          envBackupGarbageCollectionLooperEnv
          (mc >>= confBackupGarbageCollectionLooperConfiguration)
  let settingFileMigrationLooperSettings =
        deriveLooperSettings
          (minutes 2)
          (hours 24)
          flagFileMigrationLooperFlags
          envFileMigrationLooperEnv
          (mc >>= confFileMigrationLooperConfiguration)
  let settingAdmin = flagAdmin <|> envAdmin <|> (mc >>= confAdmin)
  let settingMonetisationSettings =
        combineToMonetisationSettings
          flagMonetisationFlags
          envMonetisationEnv
          (mc >>= confMonetisationConf)
  pure Settings {..}

combineToMonetisationSettings :: MonetisationFlags -> MonetisationEnvironment -> Maybe MonetisationConfiguration -> Maybe MonetisationSettings
combineToMonetisationSettings MonetisationFlags {..} MonetisationEnvironment {..} mc =
  MonetisationSettings
    <$> (monetisationFlagStripeSecretKey <|> monetisationEnvStripeSecretKey <|> (mc >>= monetisationConfStripeSecretKey))
    <*> (monetisationFlagStripePublishableKey <|> monetisationEnvStripePublishableKey <|> (mc >>= monetisationConfStripePublishableKey))
    <*> (monetisationFlagStripePrice <|> monetisationEnvStripePrice <|> (mc >>= monetisationConfStripePrice))
    <*> pure (S.unions [monetisationFlagFreeloaders, monetisationEnvFreeloaders, maybe S.empty monetisationConfFreeloaders mc])

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Enviromnent") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SMOS_SERVER_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var (left Env.UnreadError . parseLogLevel) "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
      <*> optional (Env.var Env.str "UUID_FILE" (Env.help "The file to store the server uuid in"))
      <*> optional (Env.var Env.str "DATABASE_FILE" (Env.help "The file to store the server database in"))
      <*> optional (Env.var Env.str "SIGNING_KEY_FILE" (Env.help "The file to store the JWT signing key in"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve web requests on"))
      <*> optional (Env.var Env.auto "MAX_BACKUPS_PER_USER" (Env.help "The maximum number of backups per user"))
      <*> optional (Env.var Env.auto "MAX_BACKUP_SIZE_PER_USER" (Env.help "The maximum number of bytes that backups can take up per user"))
      <*> optional (Env.var (fmap (fromIntegral :: Int -> NominalDiffTime) . Env.auto) "BACKUP_INTERVAL" (Env.help "The interval between automatic backups (seconds)"))
      <*> looperEnvironmentParser "AUTO_BACKUP"
      <*> looperEnvironmentParser "BACKUP_GARBAGE_COLLECTOR"
      <*> looperEnvironmentParser "FILE_MIGRATOR"
      <*> optional (Env.var (left Env.UnreadError . parseUsernameWithError . T.pack) "ADMIN" (Env.help "The user that will have admin rights"))
      <*> monetisationEnvironmentParser

monetisationEnvironmentParser :: Env.Parser Env.Error MonetisationEnvironment
monetisationEnvironmentParser =
  MonetisationEnvironment
    <$> optional (Env.var Env.str "STRIPE_SECRET_KEY" (Env.help "The stripe api secret key"))
    <*> optional (Env.var Env.str "STRIPE_PUBLISHABLE_KEY" (Env.help "The stripe api publishable key"))
    <*> optional (Env.var Env.str "STRIPE_PRICE" (Env.help "The stripe price id"))
    <*> Env.var
      ( left Env.UnreadError
          . fmap S.fromList
          . mapM (parseUsernameWithError . T.strip)
          . T.splitOn ","
          . T.pack
      )
      "FREELOADERS"
      (Env.def S.empty <> Env.help "The usernames of users that will not have to pay, comma separated")

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readYamlConfigFile

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) help_
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

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "The config file to use",
                metavar "FILEPATH"
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
    <*> parseMonetisationFlags

parseMonetisationFlags :: Parser MonetisationFlags
parseMonetisationFlags =
  MonetisationFlags
    <$> optional
      ( strOption
          ( mconcat
              [ long "stripe-secret-key",
                metavar "SECRET_KEY",
                help "The stripe api secret key"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "stripe-publishable-key",
                metavar "PUBLISHABLE_KEY",
                help "The stripe api publishable key"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "stripe-price",
                metavar "PRICE_ID",
                help "The stripe price id"
              ]
          )
      )
    <*> ( S.fromList
            <$> many
              ( option
                  (eitherReader $ parseUsernameWithError . T.pack)
                  ( mconcat
                      [ long "freeloader",
                        metavar "USERNAME",
                        help "The username of a user that will not have to pay"
                      ]
                  )
              )
        )
