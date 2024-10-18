{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.OptParse
  ( Settings (..),
    MonetisationSettings (..),
    getSettings,
  )
where

import Autodocodec
import Control.Monad.Logger
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Word
import Looper
import OptEnvConf
import Path
import Paths_smos_server (version)
import Smos.API
import Smos.CLI.Logging ()
import Smos.Server.Looper.BackupGarbageCollector

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' API server"

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

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = subEnv_ "smos-server" $ withLocalYamlConfig $ do
  settingLogLevel <- settingsParser
  settingUUIDFile <-
    filePathSetting
      [ help "The file to store the server uuid in",
        name "uuid-file",
        value "smos-server-uuid.json"
      ]
  settingDatabaseFile <-
    filePathSetting
      [ help "The file to store the server database in",
        name "database-file",
        value "smos-server-database.sqlite3"
      ]
  settingSigningKeyFile <-
    filePathSetting
      [ help "The file to store the JWT signing key in",
        name "signing-key-file",
        value "smos-signing-key.json"
      ]
  settingPort <-
    setting
      [ help "The port to serve web requests on",
        reader auto,
        name "port",
        value 8000,
        metavar "PORT"
      ]
  settingMaxBackupsPerPeriodPerUser <-
    setting
      [ help "The maximum number of bytes that backups can take up per user",
        confWith
          "max-backups-per-user-per-period"
          ( singleOrListCodec $
              object "Period" $
                (,)
                  <$> requiredField "period" "period, in seconds"
                    .= fst
                  <*> requiredField "max-backups" "maximum backups in this period"
                    .= snd
          ),
        value defaultPeriods
      ]
  settingMaxBackupSizePerUser <-
    optional $
      setting
        [ help "The maximum number of bytes that backups can take up per user",
          reader auto,
          name "max-backup-size-per-user",
          metavar "BYTES"
        ]
  settingAutoBackupLooperSettings <-
    parseLooperSettings
      "auto-backup"
      (seconds 30)
      (hours 1)
  settingBackupGarbageCollectionLooperSettings <-
    parseLooperSettings
      "backup-garbage-collector"
      (minutes 1)
      (hours 1)
  settingFileMigrationLooperSettings <-
    parseLooperSettings
      "file-migrator"
      (minutes 2)
      (hours 24)
  settingAdmin <-
    optional $
      setting
        [ help "The user that will have admin rights",
          reader $ eitherReader $ parseUsernameWithError . T.pack,
          name "admin",
          metavar "USERNAMES"
        ]
  settingBookingEmailAddress <-
    optional $
      setting
        [ help "Email address to send booking emails from",
          reader str,
          name "booking-email-address",
          metavar "EMAIL_ADDRESS"
        ]
  settingMonetisationSettings <- optional $ subSettings "monetisation"
  pure Settings {..}

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSecretKey :: !Text,
    monetisationSetStripePublishableKey :: !Text,
    monetisationSetStripePrice :: !Text,
    monetisationSetFreeloaders :: !(Set Username)
  }
  deriving (Show)

instance HasParser MonetisationSettings where
  settingsParser = parseMonetisationSettings

{-# ANN parseMonetisationSettings ("NOCOVER" :: String) #-}
parseMonetisationSettings :: OptEnvConf.Parser MonetisationSettings
parseMonetisationSettings = do
  monetisationSetStripeSecretKey <-
    setting
      [ help "The stripe api secret key",
        reader str,
        name "stripe-secret-key",
        metavar "SECRET_KEY"
      ]
  monetisationSetStripePublishableKey <-
    setting
      [ help "The stripe api publishable key",
        reader str,
        name "stripe-publishable-key",
        metavar "PUBLISHABLE_KEY"
      ]
  monetisationSetStripePrice <-
    setting
      [ help "The stripe price id",
        reader str,
        name "stripe-price",
        metavar "PRICE_ID"
      ]
  monetisationSetFreeloaders <-
    setting
      [ help "The usernames of users that will not have to pay, comma separated",
        reader $ commaSeparatedSet $ eitherReader $ parseUsernameWithError . T.strip . T.pack,
        name "freeloaders",
        metavar "USERNAME"
      ]
  pure MonetisationSettings {..}
