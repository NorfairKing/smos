{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.OptParse
  ( module Smos.Sync.Client.OptParse,
    module Smos.Sync.Client.OptParse.Types,
  )
where

import Control.Monad.Logger
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
import Smos.Client
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Sync.Client.OptParse.Types
import qualified System.Environment as System
import System.Exit (die)

getInstructions :: IO Instructions
getInstructions = do
  Arguments c flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions c (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions c Flags {..} Environment {..} mc = do
  dc <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
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
            "No sync server configured. Set sync { server-url: \'YOUR_SYNC_SERVER_URL\' in the config file."
        Just s -> Servant.parseBaseUrl s
    let setLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> cM syncConfLogLevel
    let setUsername = flagUsername <|> envUsername <|> cM syncConfUsername
    setPassword <-
      case flagPassword of
        Just p -> do
          putStrLn "WARNING: Plaintext password in flags may end up in shell history."
          pure (Just p)
        Nothing ->
          case envPassword of
            Just p -> pure (Just p)
            Nothing ->
              case cM syncConfPassword of
                Just p -> do
                  putStrLn "WARNING: Plaintext password in config file."
                  pure (Just p)
                Nothing -> pure Nothing
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
            Nothing -> Report.resolveDirWorkflowDir dc
            Just d -> resolveDir' d
        syncSetUUIDFile <-
          case syncFlagUUIDFile <|> envUUIDFile <|> cM syncConfUUIDFile of
            Nothing -> resolveFile dataDir "server-uuid.json"
            Just d -> resolveFile' d
        syncSetMetadataDB <-
          case syncFlagMetadataDB <|> envMetadataDB <|> cM syncConfMetadataDB of
            Nothing -> resolveFile dataDir "sync-metadata.sqlite3"
            Just d -> resolveFile' d
        syncSetBackupDir <- case syncFlagBackupDir <|> envBackupDir <|> cM syncConfBackupDir of
          Nothing -> resolveDir dataDir "conflict-backups"
          Just d -> resolveDir' d
        let syncSetIgnoreFiles =
              fromMaybe IgnoreHiddenFiles $
                syncFlagIgnoreFiles <|> envIgnoreFiles <|> cM syncConfIgnoreFiles
        case stripProperPrefix syncSetContentsDir syncSetMetadataDB of
          Nothing -> pure ()
          Just _ -> die "The metadata database must not be in the sync contents directory."
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

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment
      <$> Report.directoryEnvironmentParser
      <*> Env.var (fmap Just . logLevelReader) "LOG_LEVEL" (mE <> Env.help "log level")
      <*> Env.var (fmap Just . Env.str) "SERVER_URL" (mE <> Env.help "The url of the server to sync with")
      <*> Env.var (fmap Just . Env.str) "CONTENTS_DIR" (mE <> Env.help "The path to the directory to sync")
      <*> Env.var (fmap Just . Env.str) "UUID_FILE" (mE <> Env.help "The path to the uuid file of the server")
      <*> Env.var (fmap Just . Env.str) "METADATA_DATABASE" (mE <> Env.help "The path to the database of metadata")
      <*> Env.var (fmap Just . ignoreFilesReader) "IGNORE_FILES" (mE <> Env.help "Which files to ignore")
      <*> Env.var (fmap Just . usernameReader) "USERNAME" (mE <> Env.help "The username to sync with")
      <*> Env.var (fmap (Just . mkPassword) . Env.str) "PASSWORD" (mE <> Env.help "The password to sync with")
      <*> Env.var (fmap Just . Env.str) "SESSION_PATH" (mE <> Env.help "The path to the file in which to store the auth session")
      <*> Env.var (fmap Just . Env.str) "BACKUP_DIR" (mE <> Env.help "The directory to store backups in when a sync conflict happens")
  where
    logLevelReader s = case parseLogLevel s of
      Nothing -> Left $ Env.UnreadError $ "Unknown log level: " <> s
      Just ll -> pure ll
    ignoreFilesReader s =
      case s of
        "nothing" -> pure IgnoreNothing
        "no" -> pure IgnoreNothing
        "hidden" -> pure IgnoreHiddenFiles
        _ -> Left $ Env.UnreadError $ "Unknown 'IgnoreFiles' value: " <> s
    usernameReader s =
      case parseUsername (T.pack s) of
        Nothing -> Left $ Env.UnreadError $ "Invalid username: " <> s
        Just un -> pure un
    mE = Env.def Nothing <> Env.keep

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration

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
        map Doc.text $
          concat
            [ [ "",
                "Smos Sync Client version: " <> showVersion version,
                ""
              ],
              readWriteDataVersionsHelpMessage,
              clientVersionsHelpMessage
            ]

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> Report.parseFlagsWithConfigFile parseFlags

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
                <$> option
                  (Just <$> str)
                  (mconcat [long "contents-dir", help "The directory to synchronise", value Nothing])
                <*> option
                  (Just <$> str)
                  (mconcat [long "uuid-file", help "The file to store the server uuid in", value Nothing])
                <*> option
                  (Just <$> str)
                  ( mconcat
                      [ long "metadata-db",
                        help "The file to store the synchronisation metadata database in",
                        value Nothing
                      ]
                  )
                <*> parseIgnoreFilesFlag
                <*> option
                  (Just <$> str)
                  (mconcat [long "backup-dir", help "The directory to store backups in when a sync conflict happens", value Nothing])
            )

parseIgnoreFilesFlag :: Parser (Maybe IgnoreFiles)
parseIgnoreFilesFlag =
  flag' (Just IgnoreNothing) (mconcat [long "ignore-nothing", help "Do not ignore hidden files"])
    <|> flag' (Just IgnoreHiddenFiles) (mconcat [long "ignore-hidden-files", help "Ignore hidden files"])
    <|> pure Nothing

parseFlags :: Parser Flags
parseFlags =
  Flags <$> Report.parseDirectoryFlags
    <*> option
      (Just <$> maybeReader parseLogLevel)
      ( mconcat
          [ long "log-level",
            help $
              unwords
                [ "The log level to use, options:",
                  show $ map renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
                ],
            value Nothing
          ]
      )
    <*> option (Just <$> str) (mconcat [long "server-url", help "The server to sync with", value Nothing])
    <*> option
      (Just <$> maybeReader (parseUsername . T.pack))
      (mconcat [long "username", help "The username to login to the sync server", value Nothing])
    <*> option
      (Just . mkPassword <$> str)
      ( mconcat
          [ long "password",
            help $
              unlines
                [ "The password to login to the sync server",
                  "WARNING: You are trusting the system that you run this command on if you pass in the password via command-line arguments."
                ],
            value Nothing
          ]
      )
    <*> option
      (Just <$> str)
      (mconcat [metavar "DIRECTORY", long "data-dir", help "The directory to store state metadata in (not the contents to be synced)", value Nothing])
    <*> option
      (Just <$> str)
      (mconcat [metavar "DIRECTORY", long "cache-dir", help "The directory to cache state data in", value Nothing])
    <*> option
      (Just <$> str)
      (mconcat [metavar "FILEPATH", long "session-path", help "The path to store the login session", value Nothing])
