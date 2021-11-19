{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import Looper
import Path
import Smos.API
import Text.Read

data Arguments
  = Arguments Command Flags
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagLogLevel :: !(Maybe LogLevel),
    serveFlagUUIDFile :: !(Maybe FilePath),
    serveFlagDatabaseFile :: !(Maybe FilePath),
    serveFlagSigningKeyFile :: !(Maybe FilePath),
    serveFlagPort :: !(Maybe Int),
    serveFlagMaxBackupsPerUser :: !(Maybe Word),
    serveFlagMaxBackupSizePerUser :: !(Maybe Word64),
    serveFlagBackupInterval :: !(Maybe NominalDiffTime),
    serveFlagAutoBackupLooperFlags :: !LooperFlags,
    serveFlagBackupGarbageCollectionLooperFlags :: !LooperFlags,
    serveFlagFileMigrationLooperFlags :: !LooperFlags,
    serveFlagAdmin :: !(Maybe Username),
    serveFlagMonetisationFlags :: !MonetisationFlags
  }
  deriving (Show, Eq, Generic)

data MonetisationFlags = MonetisationFlags
  { monetisationFlagStripeSecretKey :: !(Maybe Text),
    monetisationFlagStripePublishableKey :: !(Maybe Text),
    monetisationFlagStripePrice :: !(Maybe Text),
    monetisationFlagFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel),
    envUUIDFile :: !(Maybe FilePath),
    envDatabaseFile :: !(Maybe FilePath),
    envSigningKeyFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envMaxBackupsPerUser :: !(Maybe Word),
    envMaxBackupSizePerUser :: !(Maybe Word64),
    envBackupInterval :: !(Maybe NominalDiffTime),
    envAutoBackupLooperEnv :: !LooperEnvironment,
    envBackupGarbageCollectionLooperEnv :: !LooperEnvironment,
    envFileMigrationLooperEnv :: !LooperEnvironment,
    envAdmin :: !(Maybe Username),
    envMonetisationEnv :: !MonetisationEnvironment
  }
  deriving (Show, Eq, Generic)

data MonetisationEnvironment = MonetisationEnvironment
  { monetisationEnvStripeSecretKey :: !(Maybe Text),
    monetisationEnvStripePublishableKey :: !(Maybe Text),
    monetisationEnvStripePrice :: !(Maybe Text),
    monetisationEnvFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confUUIDFile :: !(Maybe FilePath),
    confDatabaseFile :: !(Maybe FilePath),
    confSigningKeyFile :: !(Maybe FilePath),
    confPort :: !(Maybe Int),
    confMaxBackupsPerUser :: !(Maybe Word),
    confMaxBackupSizePerUser :: !(Maybe Word64),
    confBackupInterval :: !(Maybe NominalDiffTime),
    confAutoBackupLooperConfiguration :: !(Maybe LooperConfiguration),
    confBackupGarbageCollectionLooperConfiguration :: !(Maybe LooperConfiguration),
    confFileMigrationLooperConfiguration :: !(Maybe LooperConfiguration),
    confAdmin :: !(Maybe Username),
    confMonetisationConf :: !(Maybe MonetisationConfiguration)
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec Configuration where
  codec = object "Configuration" configurationObjectCodec

configurationObjectCodec :: JSONObjectCodec Configuration
configurationObjectCodec =
  Configuration
    <$> optionalFieldOrNullWith
      "log-level"
      (bimapCodec parseLogLevel renderLogLevel codec)
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
    <*> optionalFieldOrNull
      "max-backups-per-user"
      "The maximum number of backups per user"
      .= confMaxBackupsPerUser
    <*> optionalFieldOrNull
      "max-backup-size-per-user"
      "The maximum number of bytes that backups can take up per user"
      .= confMaxBackupSizePerUser
    <*> optionalFieldOrNullWith
      "backup-interval"
      (dimapCodec (fromIntegral :: Int -> NominalDiffTime) round codec)
      "The interval between automatic backups (seconds)"
      .= confBackupInterval
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
  deriving (Show, Eq, Generic)

instance HasCodec MonetisationConfiguration where
  codec =
    object "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalFieldOrNull "stripe-secret-key" "The secret key for calling the stripe api" .= monetisationConfStripeSecretKey
        <*> optionalFieldOrNull "stripe-publishable-key" "The publishable key for calling the stripe api" .= monetisationConfStripePublishableKey
        <*> optionalFieldOrNull "stripe-price" "The stripe identifier of the stripe price used to checkout a subscription" .= monetisationConfStripePrice
        <*> optionalFieldOrNullWithOmittedDefault "freeloaders" S.empty "The usernames of users that will not have to pay" .= monetisationConfFreeloaders

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings = ServeSettings
  { serveSetLogLevel :: !LogLevel,
    serveSetUUIDFile :: !(Path Abs File),
    serveSetDatabaseFile :: !(Path Abs File),
    serveSetSigningKeyFile :: !(Path Abs File),
    serveSetPort :: !Int,
    serveSetMaxBackupsPerUser :: !(Maybe Word),
    serveSetMaxBackupSizePerUser :: !(Maybe Word64),
    serveSetBackupInterval :: NominalDiffTime,
    serveSetAutoBackupLooperSettings :: !LooperSettings,
    serveSetBackupGarbageCollectionLooperSettings :: !LooperSettings,
    serveSetFileMigrationLooperSettings :: !LooperSettings,
    serveSetAdmin :: !(Maybe Username),
    serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
  }
  deriving (Show, Eq, Generic)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSecretKey :: !Text,
    monetisationSetStripePublishableKey :: !Text,
    monetisationSetStripePrice :: !Text,
    monetisationSetFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
