{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.OptParse
  ( module Smos.Sync.Client.OptParse,
    module Smos.Sync.Client.OptParse.Types,
  )
where

import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Path.IO
import Paths_smos_sync_client
import Servant.Client as Servant
import Smos.API
import Smos.CLI.Logging
import Smos.CLI.OptParse as CLI
import Smos.CLI.Password
import Smos.Client
import Smos.Data
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.Sync.Client.OptParse.Types
import qualified System.Environment as System
import System.Exit (die)

getInstructions :: IO Instructions
getInstructions = do
  Arguments c flags <- getArguments
  env <- getEnvironment
  config <- CLI.getConfiguration flags env
  combineToInstructions c (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions c Flags {..} Environment {..} mc = do
  dc <-
    combineToDirectorySettings
      defaultDirectorySettings
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConf <$> mc)
  cacheDir <- defaultCacheDir $ flagCacheDir <|> cM syncConfCacheDir
  dataDir <- defaultDataDir $ flagDataDir <|> cM syncConfDataDir
  s <- do
    setServerUrl <-
      case flagServerUrl <|> envServerUrl <|> cM syncConfServerUrl of
        Nothing ->
          die
            "No sync server configured. Set server-url: \'YOUR_SYNC_SERVER_URL\' in the config file."
        Just s -> Servant.parseBaseUrl s
    let setLogLevel = combineLogLevelSettings flagLogLevel envLogLevel (cM syncConfLogLevel)
    let setUsername = flagUsername <|> envUsername <|> cM syncConfUsername
    setPassword <-
      combinePasswordSettingsWithLogLevel
        setLogLevel
        flagPassword
        flagPasswordFile
        envPassword
        envPasswordFile
        (cM syncConfPassword)
        (cM syncConfPasswordFile)
    setSessionPath <-
      case flagSessionPath <|> envSessionPath <|> cM syncConfSessionPath of
        Nothing -> resolveFile cacheDir "sync-session.dat"
        Just f -> resolveFile' f
    pure $ Settings {..}
  d <-
    case c of
      CommandRegister RegisterFlags -> pure $ DispatchRegister RegisterSettings
      CommandLogin LoginFlags -> pure $ DispatchLogin LoginSettings
      CommandSync SyncFlags {..} -> do
        syncSetContentsDir <-
          case syncFlagContentsDir <|> envContentsDir <|> cM syncConfContentsDir of
            Nothing -> resolveDirWorkflowDir dc
            Just d -> resolveDir' d
        syncSetMetadataDB <-
          case syncFlagMetadataDB <|> envMetadataDB <|> cM syncConfMetadataDB of
            Nothing -> resolveFile dataDir "sync-metadata.sqlite3"
            Just d -> resolveFile' d
        case stripProperPrefix syncSetContentsDir syncSetMetadataDB of
          Nothing -> pure ()
          Just _ -> die "The metadata database must not be in the sync contents directory."
        syncSetUUIDFile <-
          case syncFlagUUIDFile <|> envUUIDFile <|> cM syncConfUUIDFile of
            Nothing -> resolveFile dataDir "server-uuid.json"
            Just d -> resolveFile' d
        syncSetBackupDir <- case syncFlagBackupDir <|> envBackupDir <|> cM syncConfBackupDir of
          Nothing -> resolveDir dataDir "conflict-backups"
          Just d -> resolveDir' d
        let syncSetIgnoreFiles =
              fromMaybe IgnoreHiddenFiles $
                syncFlagIgnoreFiles <|> envIgnoreFiles <|> cM syncConfIgnoreFiles
        let syncSetEmptyDirs =
              fromMaybe RemoveEmptyDirs $
                syncFlagEmptyDirs <|> envEmptyDirs <|> cM syncConfEmptyDirs
        pure $ DispatchSync SyncSettings {..}
  pure $ Instructions d s
  where
    cM :: (SyncConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSyncConf >>= func

smosRelDir :: Path Rel Dir
smosRelDir = [reldir|smos|]

defaultDataDir :: Maybe FilePath -> IO (Path Abs Dir)
defaultDataDir md = case md of
  Nothing -> getXdgDir XdgData (Just smosRelDir)
  Just fp -> resolveDir' fp

defaultCacheDir :: Maybe FilePath -> IO (Path Abs Dir)
defaultCacheDir md = case md of
  Nothing -> getXdgDir XdgCache (Just smosRelDir)
  Just fp -> resolveDir' fp

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> directoryEnvironmentParser
      <*> optional (Env.var logLevelEnvParser "LOG_LEVEL" (Env.help "log level"))
      <*> optional (Env.var Env.str "SERVER_URL" (Env.help "The url of the server to sync with"))
      <*> optional (Env.var Env.str "CONTENTS_DIR" (Env.help "The path to the directory to sync"))
      <*> optional (Env.var Env.str "UUID_FILE" (Env.help "The path to the uuid file of the server"))
      <*> optional (Env.var Env.str "METADATA_DATABASE" (Env.help "The path to the database of metadata"))
      <*> optional (Env.var ignoreFilesReader "IGNORE_FILES" (Env.help "Which files to ignore"))
      <*> optional (Env.var emptyDirsReader "EMPTY_DIRS" (Env.help "What to do with empty directories after syncing"))
      <*> optional (Env.var usernameReader "USERNAME" (Env.help "The username to sync with"))
      <*> optional (Env.var (fmap mkPassword . Env.str) "PASSWORD" (Env.help "The password to sync with"))
      <*> optional (Env.var Env.str "PASSWORD_FILE" (Env.help "The password file to sync with"))
      <*> optional (Env.var Env.str "SESSION_PATH" (Env.help "The path to the file in which to store the auth session"))
      <*> optional (Env.var Env.str "BACKUP_DIR" (Env.help "The directory to store backups in when a sync conflict happens"))
  where
    ignoreFilesReader s =
      case s of
        "nothing" -> pure IgnoreNothing
        "no" -> pure IgnoreNothing
        "hidden" -> pure IgnoreHiddenFiles
        _ -> Left $ Env.UnreadError $ "Unknown 'IgnoreFiles' value: " <> s
    emptyDirsReader s =
      case s of
        "remove" -> pure RemoveEmptyDirs
        "keep" -> pure KeepEmptyDirs
        _ -> Left $ Env.UnreadError $ "Unknown 'EmptyDirs' value: " <> s
    usernameReader s =
      case parseUsername (T.pack s) of
        Nothing -> Left $ Env.UnreadError $ "Invalid username: " <> s
        Just un -> pure un

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.pretty $
          concat
            [ [ "",
                "Smos Sync Client version: " <> showVersion version,
                ""
              ],
              readWriteDataVersionsHelpMessage,
              clientVersionsHelpMessage
            ]

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlagsWithConfigFile parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "register" parseCommandRegister,
        command "login" parseCommandLogin,
        command "sync" parseCommandSync
      ]

parseCommandRegister :: ParserInfo Command
parseCommandRegister = info parser modifier
  where
    modifier = fullDesc <> progDesc "Register at a sync server"
    parser = pure $ CommandRegister RegisterFlags

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login at a sync server"
    parser = pure $ CommandLogin LoginFlags

parseCommandSync :: ParserInfo Command
parseCommandSync = info parser modifier
  where
    modifier = fullDesc <> progDesc "Sync with a sync server"
    parser =
      CommandSync
        <$> ( SyncFlags
                <$> optional
                  ( strOption
                      ( mconcat
                          [ long "contents-dir",
                            help "The directory to synchronise"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "uuid-file",
                            help "The file to store the server uuid in"
                          ]
                      )
                  )
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "metadata-db",
                            help "The file to store the synchronisation metadata database in"
                          ]
                      )
                  )
                <*> parseIgnoreFilesFlag
                <*> parseEmptyDirsFlag
                <*> optional
                  ( strOption
                      ( mconcat
                          [ long "backup-dir",
                            help "The directory to store backups in when a sync conflict happens"
                          ]
                      )
                  )
            )

parseIgnoreFilesFlag :: Parser (Maybe IgnoreFiles)
parseIgnoreFilesFlag =
  optional $
    flag'
      IgnoreNothing
      ( mconcat
          [ long "ignore-nothing",
            help "Do not ignore hidden files"
          ]
      )
      <|> flag'
        IgnoreHiddenFiles
        ( mconcat
            [ long "ignore-hidden-files",
              help "Ignore hidden files"
            ]
        )

parseEmptyDirsFlag :: Parser (Maybe EmptyDirs)
parseEmptyDirsFlag =
  optional $
    flag'
      RemoveEmptyDirs
      ( mconcat
          [ long "remove-empty-dirs",
            help "Remove empty directories after syncing"
          ]
      )
      <|> flag'
        KeepEmptyDirs
        ( mconcat
            [ long "keep-empty-dirs",
              help "Keep empty directories after syncing"
            ]
        )

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> parseDirectoryFlags
    <*> parseLogLevelOption
    <*> optional (strOption (mconcat [long "server-url", help "The server to sync with"]))
    <*> optional
      ( option
          (maybeReader (parseUsername . T.pack))
          (mconcat [long "username", help "The username to login to the sync server"])
      )
    <*> optional
      ( option
          (mkPassword <$> str)
          ( mconcat
              [ long "password",
                help $
                  unlines
                    [ "The password to login to the sync server",
                      "WARNING: You are trusting the system that you run this command on if you pass in the password via command-line arguments."
                    ]
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "password-file",
                help "The password file to login to the sync server"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ metavar "DIRECTORY",
                long "data-dir",
                help "The directory to store state metadata in (not the contents to be synced)"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ metavar "DIRECTORY",
                long "cache-dir",
                help "The directory to cache state data in"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ metavar "FILEPATH",
                long "session-path",
                help "The path to store the login session"
              ]
          )
      )
