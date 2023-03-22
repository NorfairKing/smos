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
import Smos.CLI.Logging ()

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
    flagMonetisationFlags :: !MonetisationFlags
  }
  deriving (Show, Eq, Generic)

data MonetisationFlags = MonetisationFlags
  { monetisationFlagStripeSecretKey :: !(Maybe Text),
    monetisationFlagStripePublishableKey :: !(Maybe Text),
    monetisationFlagStripePrice :: !(Maybe Text),
    monetisationFlagFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)

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
    confMaxBackupsPerPeriodPerUser :: !(Maybe [(NominalDiffTime, Word)]),
    confMaxBackupSizePerUser :: !(Maybe Word64),
    confAutoBackupLooperConfiguration :: !(Maybe LooperConfiguration),
    confBackupGarbageCollectionLooperConfiguration :: !(Maybe LooperConfiguration),
    confFileMigrationLooperConfiguration :: !(Maybe LooperConfiguration),
    confAdmin :: !(Maybe Username),
    confMonetisationConf :: !(Maybe MonetisationConfiguration)
  }
  deriving stock (Show, Eq, Generic)

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
                <$> requiredField "period" "period, in seconds" .= fst
                <*> requiredField "max-backups" "maximum backups in this period" .= snd
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
    settingMonetisationSettings :: !(Maybe MonetisationSettings)
  }
  deriving (Show, Eq, Generic)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSecretKey :: !Text,
    monetisationSetStripePublishableKey :: !Text,
    monetisationSetStripePrice :: !Text,
    monetisationSetFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)
