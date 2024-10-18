{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.OptParse where

import Autodocodec
import Control.Monad.Logger
import qualified Data.Text as T
import OptEnvConf
import Path
import Path.IO
import Paths_smos_sync_client (version)
import Servant.Client as Servant
import Smos.API
import Smos.CLI.OptParse
import Smos.Directory.Resolution

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Smos' sync client"

data Instructions
  = Instructions
      !Dispatch
      !Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchRegister
  | DispatchLogin
  | DispatchSync !SyncSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "register" "Register at a sync server" $
          pure DispatchRegister,
        command "login" "Login at a sync server" $
          pure DispatchLogin,
        command "sync" "Sync with a sync server" $
          DispatchSync <$> settingsParser,
        defaultCommand "sync"
      ]

data SyncSettings = SyncSettings
  { syncSetContentsDir :: !(Path Abs Dir),
    syncSetUUIDFile :: !(Path Abs File),
    syncSetMetadataDB :: !(Path Abs File),
    syncSetBackupDir :: !(Path Abs Dir),
    syncSetIgnoreFiles :: !IgnoreFiles,
    syncSetEmptyDirs :: !EmptyDirs
  }
  deriving (Show)

instance HasParser SyncSettings where
  settingsParser = parseSyncSettings

{-# ANN parseSyncSettings ("NOCOVER" :: String) #-}
parseSyncSettings :: OptEnvConf.Parser SyncSettings
parseSyncSettings = checkMapEither
  ( \ss ->
      case stripProperPrefix (syncSetContentsDir ss) (syncSetMetadataDB ss) of
        Nothing -> Right ss
        Just _ -> Left "The metadata database must not be in the sync contents directory."
  )
  $ do
    let sub = subConfig_ "sync" . subEnv_ "sync-client"
    syncSetContentsDir <-
      mapIO
        ( \(dc, mcd) -> do
            case mcd of
              Nothing -> resolveWorkflowDir dc
              Just d -> pure d
        )
        $ (,)
          <$> settingsParser
          <*> optional
            ( sub $
                directoryPathSetting
                  [ help "The directory to synchronise",
                    name "contents-dir"
                  ]
            )
    ~(syncSetUUIDFile, syncSetMetadataDB, syncSetBackupDir) <-
      sub
        $ mapIO
          ( \(dd, uf, mf, bd) -> do
              syncSetUUIDFile <- resolveFile dd uf
              syncSetMetadataDB <- resolveFile dd mf
              syncSetBackupDir <- resolveDir dd bd
              pure (syncSetUUIDFile, syncSetMetadataDB, syncSetBackupDir)
          )
        $ (,,,)
          <$> choice
            [ directoryPathSetting
                [ help "The directory to store state data in",
                  name "data-dir"
                ],
              runIO $ getXdgDir XdgData (Just smosRelDir)
            ]
          <*> setting
            [ help "The file to store the server uuid in",
              reader str,
              name "uuid-file",
              value "server-uuid.json",
              metavar "FILE_PATH"
            ]
          <*> setting
            [ help "The file to store the synchronisation metadata database in",
              reader str,
              name "metadata-db",
              value "sync-metadata.sqlite3",
              metavar "FILE_PATH"
            ]
          <*> setting
            [ help "The directory to store backups in when a sync conflict happens",
              reader str,
              name "backup-dir",
              value "conflict-backups",
              metavar "FILE_PATH"
            ]
    syncSetIgnoreFiles <- sub settingsParser
    syncSetEmptyDirs <- sub settingsParser
    pure SyncSettings {..}

data IgnoreFiles
  = IgnoreNothing
  | IgnoreHiddenFiles
  deriving (Show, Eq)

instance HasCodec IgnoreFiles where
  codec =
    stringConstCodec
      [ (IgnoreNothing, "nothing"),
        (IgnoreHiddenFiles, "hidden")
      ]
      <??> [ "nothing: Don't ignore any files",
             "hidden: Ignore hidden files"
           ]

instance HasParser IgnoreFiles where
  settingsParser =
    let h = help "Which files to ignore"
     in choice
          [ setting
              [ help "Do not ignore hidden files",
                switch IgnoreNothing,
                long "ignore-nothing"
              ],
            setting
              [ help "Ignore hidden files",
                switch IgnoreHiddenFiles,
                long "ignore-hidden-files"
              ],
            setting
              [ h,
                reader $ eitherReader $ \case
                  "nothing" -> pure IgnoreNothing
                  "no" -> pure IgnoreNothing
                  "hidden" -> pure IgnoreHiddenFiles
                  s -> Left $ unwords ["Unknown 'IgnoreFiles' value:", s],
                env "IGNORE_FILES",
                metavar "IGNORE_FILES",
                example "no",
                example "nothing",
                example "hidden"
              ],
            setting
              [ h,
                conf "ignore-files"
              ],
            setting
              [ h,
                value IgnoreHiddenFiles
              ]
          ]

data EmptyDirs
  = RemoveEmptyDirs
  | KeepEmptyDirs
  deriving (Show, Eq)

instance HasCodec EmptyDirs where
  codec =
    stringConstCodec
      [ (RemoveEmptyDirs, "remove"),
        (KeepEmptyDirs, "keep")
      ]
      <??> [ "remove: Remove empty directories after syncing",
             "keep: Keep empty directories after syncing"
           ]

instance HasParser EmptyDirs where
  settingsParser =
    choice
      [ setting
          [ help "Remove empty directories after syncing",
            switch RemoveEmptyDirs,
            long "remove-empty-dirs"
          ],
        setting
          [ help "Keep empty directories after syncing",
            switch KeepEmptyDirs,
            long "keep-empty-dirs"
          ],
        setting
          [ help "What to do with empty directories after syncing",
            reader $ eitherReader $ \case
              "remove" -> pure RemoveEmptyDirs
              "keep" -> pure KeepEmptyDirs
              s -> Left $ unwords ["Unknown 'EmptyDirs' value:", s],
            env "EMPTY_DIRS",
            metavar "EMPTY_DIR"
          ],
        setting
          [ help "What to do with empty directories after syncing",
            conf "empty-directories"
          ],
        setting
          [ help "What to do with empty directories after syncing",
            value KeepEmptyDirs
          ]
      ]

data Settings = Settings
  { setServerUrl :: BaseUrl,
    setLogLevel :: LogLevel,
    setUsername :: Maybe Username,
    setPassword :: Maybe Password,
    setSessionPath :: Path Abs File
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  let sub = subConfig_ "sync" . subEnv_ "sync-client"
  setServerUrl <-
    sub $
      setting
        [ help "The server to sync with",
          reader $ maybeReader parseBaseUrl,
          name "server-url",
          metavar "URL",
          example "https://api.smos.online"
        ]
  setLogLevel <- sub settingsParser
  setUsername <-
    optional $
      sub $
        setting
          [ help "The username to login to the sync server",
            reader $ eitherReader $ parseUsernameWithError . T.pack,
            name "username",
            metavar "USERNAME"
          ]
  setPassword <-
    optional $
      sub $
        mkPassword
          <$> secretTextFileOrBareSetting
            [ help "The password to login to the sync server",
              name "password",
              metavar "PASSWORD"
            ]
  setSessionPath <-
    sub
      $ mapIO
        ( \(cd, sp) -> do
            resolveFile cd sp
        )
      $ (,)
        <$> choice
          [ directoryPathSetting
              [ help "The directory to cache state data in",
                name "cache-dir"
              ],
            runIO $ getXdgDir XdgCache (Just smosRelDir)
          ]
        <*> setting
          [ help "The path to store the login session",
            reader str,
            name "session-path",
            value "sync-session.dat",
            metavar "FILE_PATH"
          ]
  pure Settings {..}

smosRelDir :: Path Rel Dir
smosRelDir = [reldir|smos|]
