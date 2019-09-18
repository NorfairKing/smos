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

import Path
import Path.IO

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
  s <- getSettings
  d <- getDispatch s
  pure $ Instructions d s
  where
    cM :: (SyncConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSyncConf >>= func
    getDispatch Settings {..} =
      case c of
        CommandSync SyncFlags {..} -> do
          syncSetServerUrl <-
            Servant.parseBaseUrl $
            fromMaybe "sync.api.smos.cs-syd.eu" $
            syncFlagServerUrl <|> envServerUrl <|> cM syncConfServerUrl
          syncSetContentsDir <-
            case syncFlagContentsDir <|> envContentsDir <|> cM syncConfContentsDir of
              Nothing -> Report.resolveReportWorkflowDir setReportConfig
              Just d -> resolveDir' d
          syncSetMetadataFile <-
            case syncFlagMetadataFile <|> envMetadataFile <|> cM syncConfMetadataFile of
              Nothing -> defaultMetadataFile
              Just d -> resolveFile' d
          pure $ DispatchSync SyncSettings {..}
    getSettings = do
      src <-
        Report.combineToConfig
          Report.defaultReportConfig
          flagReportFlags
          envReportEnvironment
          (confReportConf <$> mc)
      pure $ Settings {setReportConfig = src}

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
            ]))

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseFlags
