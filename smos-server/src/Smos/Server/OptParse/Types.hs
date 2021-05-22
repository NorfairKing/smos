{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Word
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Looper
import Path
import Smos.API
import Text.Read
import YamlParse.Applicative

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
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = objectParser "Configuration" configurationObjectParser

configurationObjectParser :: ObjectParser Configuration
configurationObjectParser =
  Configuration
    <$> optionalFieldWith "log-level" "The minimal severity for log messages" (maybeParser parseLogLevel yamlSchema)
    <*> optionalField "uuid-file" "The file in which to store the server uuid"
    <*> optionalField "database-file" "The file in which to store the database"
    <*> optionalField "signing-key-file" "The file in which to store signing key for JWT tokens"
    <*> optionalField "port" "The port on which to serve api requests"
    <*> optionalField "max-backups-per-user" "The maximum number of backups per user"
    <*> optionalField "max-backup-size-per-user" "The maximum number of bytes that backups can take up per user"
    <*> optionalFieldWith "backup-interval" "The interval between automatic backups (seconds)" ((fromIntegral :: Int -> NominalDiffTime) <$> yamlSchema)
    <*> optionalField "auto-backup" "The configuration for the automatic backup looper"
    <*> optionalField "backup-garbage-collector" "The configuration for the automatic backup garbage collection looper"
    <*> optionalField "file-migrator" "The configuration for the automatic file format migrator looper"
    <*> optionalField "admin" "The username of the user who will have admin rights"
    <*> optionalField
      "monetisation"
      "Monetisation configuration. If this is not configured then the server is run entirely for free."

data MonetisationConfiguration = MonetisationConfiguration
  { monetisationConfStripeSecretKey :: !(Maybe Text),
    monetisationConfStripePublishableKey :: !(Maybe Text),
    monetisationConfStripePrice :: !(Maybe Text),
    monetisationConfFreeloaders :: !(Set Username)
  }
  deriving (Show, Eq, Generic)

instance FromJSON MonetisationConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema MonetisationConfiguration where
  yamlSchema =
    objectParser "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalField "stripe-secret-key" "The secret key for calling the stripe api"
        <*> optionalField "stripe-publishable-key" "The publishable key for calling the stripe api"
        <*> optionalField
          "stripe-plan"
          "The stripe identifier of the stripe plan used to checkout a subscription"
        <*> optionalFieldWithDefault "freeloaders" S.empty "The usernames of users that will not have to pay"

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

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
