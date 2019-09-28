{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Client.OptParse
  ( getInstructions
  , Instructions(..)
  , Dispatch(..)
  , SyncSettings(..)
  , Settings(..)
  ) where

import Data.Maybe

import qualified System.Environment as System
import System.Exit (die)

import Control.Monad.Logger
import Path
import Path.IO
import Text.Read

import Options.Applicative

import Servant.Client as Servant

import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report

import Smos.Sync.Client.OptParse.Types

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
        CommandSync SyncFlags {..} -> do
          syncSetServerUrl <-
            case syncFlagServerUrl <|> envServerUrl <|> cM syncConfServerUrl of
              Nothing ->
                die
                  "No sync server configured. Set sync { server-url: \'YOUR_SYNC_SERVER_URL\' in the config file."
              Just s -> Servant.parseBaseUrl s
          syncSetContentsDir <-
            case syncFlagContentsDir <|> envContentsDir <|> cM syncConfContentsDir of
              Nothing -> Report.resolveReportWorkflowDir src
              Just d -> resolveDir' d
          syncSetMetadataFile <-
            case syncFlagMetadataFile <|> envMetadataFile <|> cM syncConfMetadataFile of
              Nothing -> defaultMetadataFile
              Just d -> resolveFile' d
          let syncSetIgnoreFiles =
                fromMaybe IgnoreHiddenFiles $
                syncFlagIgnoreFiles <|> envIgnoreFiles <|> cM syncConfIgnoreFiles
          pure $ DispatchSync SyncSettings {..}
    getSettings = pure $ Settings {setLogLevel = fromMaybe LevelInfo flagLogLevel}

defaultMetadataFile :: IO (Path Abs File)
defaultMetadataFile = do
  home <- getHomeDir
  resolveFile home ".smos/sync-metadata.json"

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let getEnv :: String -> Maybe String
      getEnv key = ("SMOS_SYNC_CLIENT" ++ key) `lookup` env
      -- readEnv :: Read a => String -> Maybe a
      -- readEnv key = getEnv key >>= readMaybe
  let envServerUrl = getEnv "SERVER_URL"
      envContentsDir = getEnv "CONTENTS_DIR"
      envMetadataFile = getEnv "METADATA_FILE"
  envIgnoreFiles <-
    case getEnv "IGNORE_FILES" of
      Just "nothing" -> pure $ Just IgnoreNothing
      Just "no" -> pure $ Just IgnoreNothing
      Just "hidden" -> pure $ Just IgnoreHiddenFiles
      Just s -> fail $ "Unknown 'IgnoreFiles' value: " <> s
      Nothing -> pure Nothing
  envReportEnvironment <- Report.getEnvironment
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
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-query"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "sync" parseCommandSync]

parseCommandSync :: ParserInfo Command
parseCommandSync = info parser modifier
  where
    modifier = fullDesc <> progDesc "Sync with a sync server"
    parser =
      CommandSync <$>
      (SyncFlags <$>
       option
         (Just <$> str)
         (mconcat [long "server-url", help "The server to sync with", value Nothing]) <*>
       option
         (Just <$> str)
         (mconcat [long "contents-dir", help "The directory to synchronise", value Nothing]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "metadata-file"
            , help "The file to store synchronisation metadata in"
            , value Nothing
            ]) <*>
       parseIgnoreFilesFlag)

parseIgnoreFilesFlag :: Parser (Maybe IgnoreFiles)
parseIgnoreFilesFlag =
  flag' (Just IgnoreNothing) (mconcat [long "ignore-nothing", help "Do not ignore hidden files"]) <|>
  flag' (Just IgnoreHiddenFiles) (mconcat [long "ignore-hidden-files", help "Ignore hidden files"]) <|>
  pure Nothing

parseFlags :: Parser Flags
parseFlags =
  Flags <$> Report.parseFlags <*>
  option
    (Just <$> maybeReader parseLogLevel)
    (mconcat
       [ long "log-level"
       , help $
         unwords
           [ "The log level to use, options:"
           , show $ map renderLogLevel [LevelDebug, LevelInfo, LevelWarn, LevelError]
           ]
       , value Nothing
       ])
  where
    parseLogLevel s = readMaybe $ "Level" <> s
    renderLogLevel = drop 5 . show
