{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.OptParse
  ( getInstructions,
    Instructions (..),
    Dispatch (..),
    SyncSettings (..),
    Settings (..),
    runArgumentsParser,
  )
where

import Control.Monad
import Control.Monad.Logger
import Data.Maybe
import qualified Data.Text as T
import Options.Applicative
import Path
import Path.IO
import Servant.Client as Servant
import Smos.API
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Sync.Client.OptParse.Types
import qualified System.Environment as System
import System.Exit (die)
import YamlParse.Applicative (confDesc)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments c Flags {..}) Environment {..} mc = do
  src <-
    Report.combineToConfig
      Report.defaultReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConf <$> mc)
  s <- getSettings
  d <- getDispatch src
  pure $ Instructions d s
  where
    cM :: (SyncConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSyncConf >>= func
    getDispatch src =
      case c of
        CommandRegister RegisterFlags -> pure $ DispatchRegister RegisterSettings
        CommandLogin LoginFlags -> pure $ DispatchLogin LoginSettings
        CommandSync SyncFlags {..} -> do
          syncSetContentsDir <-
            case syncFlagContentsDir <|> envContentsDir <|> cM syncConfContentsDir of
              Nothing -> Report.resolveReportWorkflowDir src
              Just d -> resolveDir' d
          syncSetUUIDFile <-
            case syncFlagUUIDFile <|> envUUIDFile <|> cM syncConfUUIDFile of
              Nothing -> defaultUUIDFile
              Just d -> resolveFile' d
          syncSetMetadataDB <-
            case syncFlagMetadataDB <|> envMetadataDB <|> cM syncConfMetadataDB of
              Nothing -> defaultMetadataDB
              Just d -> resolveFile' d
          let syncSetIgnoreFiles =
                fromMaybe IgnoreHiddenFiles $
                  syncFlagIgnoreFiles <|> envIgnoreFiles <|> cM syncConfIgnoreFiles
          pure $ DispatchSync SyncSettings {..}
    getSettings = do
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
          Nothing -> defaultSessionPath
          Just f -> resolveFile' f
      pure $ Settings {..}

defaultUUIDFile :: IO (Path Abs File)
defaultUUIDFile = do
  home <- getHomeDir
  resolveFile home ".smos/server-uuid.json"

defaultMetadataDB :: IO (Path Abs File)
defaultMetadataDB = do
  home <- getHomeDir
  resolveFile home ".smos/sync-metadata.sqlite3"

defaultSessionPath :: IO (Path Abs File)
defaultSessionPath = do
  home <- getHomeDir
  resolveFile home ".smos/sync-session.dat"

getEnvironment :: IO Environment
getEnvironment = do
  envReportEnvironment <- Report.getEnvironment
  env <- System.getEnvironment
  let getEnv :: String -> Maybe String
      getEnv key = ("SMOS_SYNC_CLIENT" ++ key) `lookup` env
  -- readEnv :: Read a => String -> Maybe a
  -- readEnv key = getEnv key >>= readMaybe
  envLogLevel <-
    forM (getEnv "LOG_LEVEL") $ \s ->
      case parseLogLevel s of
        Nothing -> fail $ "Unknown log level: " <> s
        Just ll -> pure ll
  let envServerUrl = getEnv "SERVER_URL"
      envContentsDir = getEnv "CONTENTS_DIR"
      envUUIDFile = getEnv "UUID_FILE"
      envMetadataDB = getEnv "METADATA_DATABASE"
  envIgnoreFiles <-
    case getEnv "IGNORE_FILES" of
      Just "nothing" -> pure $ Just IgnoreNothing
      Just "no" -> pure $ Just IgnoreNothing
      Just "hidden" -> pure $ Just IgnoreHiddenFiles
      Just s -> fail $ "Unknown 'IgnoreFiles' value: " <> s
      Nothing -> pure Nothing
  envUsername <-
    forM (getEnv "USERNAME") $ \s ->
      case parseUsername (T.pack s) of
        Nothing -> fail $ "Invalid username: " <> s
        Just un -> pure un
  envPassword <-
    forM (getEnv "PASSWORD") $ \s ->
      case parsePassword (T.pack s) of
        Nothing -> fail $ "Invalid password: " <> s
        Just pw -> pure pw
  let envSessionPath = getEnv "SESSION_PATH"
  pure Environment {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  Report.getConfiguration flagReportFlags envReportEnvironment

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description <> confDesc @Configuration
    description = "smos-sync-client"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

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
    parser = CommandRegister <$> pure RegisterFlags

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login at a sync server"
    parser = CommandLogin <$> pure LoginFlags

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
            )

parseIgnoreFilesFlag :: Parser (Maybe IgnoreFiles)
parseIgnoreFilesFlag =
  flag' (Just IgnoreNothing) (mconcat [long "ignore-nothing", help "Do not ignore hidden files"])
    <|> flag' (Just IgnoreHiddenFiles) (mconcat [long "ignore-hidden-files", help "Ignore hidden files"])
    <|> pure Nothing

parseFlags :: Parser Flags
parseFlags =
  Flags <$> Report.parseFlags
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
      (Just <$> maybeReader (parsePassword . T.pack))
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
      (mconcat [long "session-path", help "The path to store the login session", value Nothing])
