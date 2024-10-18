{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Arrow
import Control.Monad.Logger
import Data.Maybe
import Data.SemVer as Version (toString)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Version
import Data.Word
import qualified Env
import Looper
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Path.IO
import Paths_smos_server
import Smos.API
import Smos.CLI.Logging
import qualified Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Server.Looper.BackupGarbageCollector
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings (CLI.flagWithRestFlags flags) (CLI.envWithRestEnv env) config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mc = do
  let settingLogLevel = combineLogLevelSettings flagLogLevel envLogLevel (mc >>= confLogLevel)
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
  let settingMaxBackupsPerPeriodPerUser = fromMaybe defaultPeriods $ mc >>= confMaxBackupsPerPeriodPerUser
  let settingMaxBackupSizePerUser = flagMaxBackupSizePerUser <|> envMaxBackupSizePerUser <|> (mc >>= confMaxBackupSizePerUser)
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
          (hours 1)
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
  let settingBookingEmailAddress = flagBookingEmailAddress <|> envBookingEmailAddress <|> (mc >>= confBookingEmailAddress)
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

getEnvironment :: IO (CLI.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Enviromnent") environmentParser

environmentParser :: Env.Parser Env.Error (CLI.EnvWithConfigFile Environment)
environmentParser =
  Env.prefixed "SMOS_SERVER_" $
    CLI.envWithConfigFileParser $
      Environment
        <$> optional (Env.var logLevelEnvParser "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
        <*> optional (Env.var Env.str "UUID_FILE" (Env.help "The file to store the server uuid in"))
        <*> optional (Env.var Env.str "DATABASE_FILE" (Env.help "The file to store the server database in"))
        <*> optional (Env.var Env.str "SIGNING_KEY_FILE" (Env.help "The file to store the JWT signing key in"))
        <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve web requests on"))
        <*> optional (Env.var Env.auto "MAX_BACKUP_SIZE_PER_USER" (Env.help "The maximum number of bytes that backups can take up per user"))
        <*> looperEnvironmentParser "AUTO_BACKUP"
        <*> looperEnvironmentParser "BACKUP_GARBAGE_COLLECTOR"
        <*> looperEnvironmentParser "FILE_MIGRATOR"
        <*> optional (Env.var (left Env.UnreadError . parseUsernameWithError . T.pack) "ADMIN" (Env.help "The user that will have admin rights"))
        <*> optional (Env.var Env.str "BOOKING_EMAIL_ADDRESS" (Env.help "Email address to send booking emails from"))
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

getConfiguration :: CLI.FlagsWithConfigFile b -> CLI.EnvWithConfigFile c -> IO (Maybe Configuration)
getConfiguration CLI.FlagsWithConfigFile {..} CLI.EnvWithConfigFile {..} =
  case flagWithConfigFile <|> envWithConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readYamlConfigFile

getFlags :: IO (CLI.FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult (CLI.FlagsWithConfigFile Flags)
runFlagsParser = CLI.execOptionParserPure flagsParser

flagsParser :: ParserInfo (CLI.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.pretty $
          [ "",
            "Smos API Server version: " <> showVersion version,
            "",
            "Server API version: " <> Version.toString apiVersion,
            ""
          ]
            ++ readDataVersionsHelpMessage

parseFlags :: Parser (CLI.FlagsWithConfigFile Flags)
parseFlags =
  CLI.parseFlagsWithConfigFile $
    Flags
      <$> parseLogLevelOption
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
                [ long "max-backup-size-per-user",
                  metavar "BYTES",
                  help "The maximum number of bytes that backups can take up per user"
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
      <*> optional
        ( strOption
            ( mconcat
                [ long "booking-email-address",
                  metavar "EMAIL_ADDRESS",
                  help "Email address to send booking emails from"
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

data Flags = Flags
  { flagLogLevel :: !(Maybe LogLevel),
    flagUUIDFile :: !(Maybe FilePath),
    flagDatabaseFile :: !(Maybe FilePath),
    flagSigningKeyFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagMaxBackupSizePerUser :: !(Maybe Word64),
    flagAutoBackupLooperFlags :: !LooperFlags,
    flagBackupGarbageCollectionLooperFlags :: !LooperFlags,
    flagFileMigrationLooperFlags :: !LooperFlags,
    flagAdmin :: !(Maybe Username),
    flagBookingEmailAddress :: !(Maybe Text),
    flagMonetisationFlags :: !MonetisationFlags
  }

data MonetisationFlags = MonetisationFlags
  { monetisationFlagStripeSecretKey :: !(Maybe Text),
    monetisationFlagStripePublishableKey :: !(Maybe Text),
    monetisationFlagStripePrice :: !(Maybe Text),
    monetisationFlagFreeloaders :: !(Set Username)
  }

data Environment = Environment
  { envLogLevel :: !(Maybe LogLevel),
    envUUIDFile :: !(Maybe FilePath),
    envDatabaseFile :: !(Maybe FilePath),
    envSigningKeyFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envMaxBackupSizePerUser :: !(Maybe Word64),
    envAutoBackupLooperEnv :: !LooperEnvironment,
    envBackupGarbageCollectionLooperEnv :: !LooperEnvironment,
    envFileMigrationLooperEnv :: !LooperEnvironment,
    envAdmin :: !(Maybe Username),
    envBookingEmailAddress :: !(Maybe Text),
    envMonetisationEnv :: !MonetisationEnvironment
  }

data MonetisationEnvironment = MonetisationEnvironment
  { monetisationEnvStripeSecretKey :: !(Maybe Text),
    monetisationEnvStripePublishableKey :: !(Maybe Text),
    monetisationEnvStripePrice :: !(Maybe Text),
    monetisationEnvFreeloaders :: !(Set Username)
  }

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confUUIDFile :: !(Maybe FilePath),
    confDatabaseFile :: !(Maybe FilePath),
    confSigningKeyFile :: !(Maybe FilePath),
    confPort :: !(Maybe Int),
    confMaxBackupsPerPeriodPerUser :: !(Maybe [(NominalDiffTime, Word)]),
    confMaxBackupSizePerUser :: !(Maybe Word64),
    confAutoBackupLooperConfiguration :: !(Maybe LooperConfiguration),
    confBackupGarbageCollectionLooperConfiguration :: !(Maybe LooperConfiguration),
    confFileMigrationLooperConfiguration :: !(Maybe LooperConfiguration),
    confAdmin :: !(Maybe Username),
    confBookingEmailAddress :: !(Maybe Text),
    confMonetisationConf :: !(Maybe MonetisationConfiguration)
  }

instance HasCodec Configuration where
  codec = object "Configuration" objectCodec

instance HasObjectCodec Configuration where
  objectCodec =
    Configuration
      <$> optionalFieldOrNull
        "log-level"
        "The minimal severity for log messages"
        .= confLogLevel
      <*> optionalFieldOrNull
        "uuid-file"
        "The file in which to store the server uuid"
        .= confUUIDFile
      <*> optionalFieldOrNull
        "database-file"
        "The file in which to store the database"
        .= confDatabaseFile
      <*> optionalFieldOrNull
        "signing-key-file"
        "The file in which to store signing key for JWT tokens"
        .= confSigningKeyFile
      <*> optionalFieldOrNull
        "port"
        "The port on which to serve api requests"
        .= confPort
      <*> optionalFieldOrNullWith
        "max-backups-per-user-per-period"
        ( singleOrListCodec $
            object "Period" $
              (,)
                <$> requiredField "period" "period, in seconds"
                  .= fst
                <*> requiredField "max-backups" "maximum backups in this period"
                  .= snd
        )
        "The maximum number of backups per user per period"
        .= confMaxBackupsPerPeriodPerUser
      <*> optionalFieldOrNull
        "max-backup-size-per-user"
        "The maximum number of bytes that backups can take up per user"
        .= confMaxBackupSizePerUser
      <*> optionalFieldOrNull
        "auto-backup"
        "The configuration for the automatic backup looper"
        .= confAutoBackupLooperConfiguration
      <*> optionalFieldOrNull
        "backup-garbage-collector"
        "The configuration for the automatic backup garbage collection looper"
        .= confBackupGarbageCollectionLooperConfiguration
      <*> optionalFieldOrNull
        "file-migrator"
        "The configuration for the automatic file format migrator looper"
        .= confFileMigrationLooperConfiguration
      <*> optionalFieldOrNull
        "admin"
        "The username of the user who will have admin rights"
        .= confAdmin
      <*> optionalFieldOrNull "booking-email-address" "Email address to send booking emails from"
        .= confBookingEmailAddress
      <*> optionalFieldOrNull
        "monetisation"
        "Monetisation configuration. If this is not configured then the server is run entirely for free."
        .= confMonetisationConf

data MonetisationConfiguration = MonetisationConfiguration
  { monetisationConfStripeSecretKey :: !(Maybe Text),
    monetisationConfStripePublishableKey :: !(Maybe Text),
    monetisationConfStripePrice :: !(Maybe Text),
    monetisationConfFreeloaders :: !(Set Username)
  }

instance HasCodec MonetisationConfiguration where
  codec =
    object "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalFieldOrNull "stripe-secret-key" "The secret key for calling the stripe api"
          .= monetisationConfStripeSecretKey
        <*> optionalFieldOrNull "stripe-publishable-key" "The publishable key for calling the stripe api"
          .= monetisationConfStripePublishableKey
        <*> optionalFieldOrNull "stripe-price" "The stripe identifier of the stripe price used to checkout a subscription"
          .= monetisationConfStripePrice
        <*> optionalFieldOrNullWithOmittedDefault "freeloaders" S.empty "The usernames of users that will not have to pay"
          .= monetisationConfFreeloaders

data Settings = Settings
  { settingLogLevel :: !LogLevel,
    settingUUIDFile :: !(Path Abs File),
    settingDatabaseFile :: !(Path Abs File),
    settingSigningKeyFile :: !(Path Abs File),
    settingPort :: !Int,
    settingMaxBackupsPerPeriodPerUser :: ![(NominalDiffTime, Word)],
    settingMaxBackupSizePerUser :: !(Maybe Word64),
    settingAutoBackupLooperSettings :: !LooperSettings,
    settingBackupGarbageCollectionLooperSettings :: !LooperSettings,
    settingFileMigrationLooperSettings :: !LooperSettings,
    settingAdmin :: !(Maybe Username),
    settingBookingEmailAddress :: !(Maybe Text),
    settingMonetisationSettings :: !(Maybe MonetisationSettings)
  }
  deriving (Show)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSecretKey :: !Text,
    monetisationSetStripePublishableKey :: !Text,
    monetisationSetStripePrice :: !Text,
    monetisationSetFreeloaders :: !(Set Username)
  }
  deriving (Show)
