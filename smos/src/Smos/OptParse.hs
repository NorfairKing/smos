{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import System.Environment (getArgs, getEnvironment)

import Options.Applicative

import Smos.OptParse.Bare
import Smos.OptParse.Types
import qualified Smos.Report.OptParse as Report

import Smos.Actions
import Smos.Types

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
    args <- getArguments
    env <- getEnv
    config <- getConfiguration args env
    combineToInstructions conf args env config

combineToInstructions ::
       SmosConfig
    -> Arguments
    -> Environment
    -> Maybe Configuration
    -> IO Instructions
combineToInstructions sc@SmosConfig {..} (Arguments fp Flags {..}) Environment {..} mc = do
    p <- resolveFile' fp
    src <-
        Report.combineToConfig
            configReportConfig
            flagReportFlags
            envReportEnv
            (confReportConf <$> mc)
    let keyMap = combineKeymap configKeyMap $ confKeybindingsConf <$> mc
    let sc' = sc {configKeyMap = keyMap, configReportConfig = src}
    pure $ Instructions p sc'

combineKeymap :: KeyMap -> Maybe KeybindingsConfiguration -> KeyMap
combineKeymap km Nothing = km
combineKeymap km (Just kbc) =
    let startingPoint =
            case confReset kbc of
                Just True -> mempty
                Just False -> km
                Nothing -> km
    in startingPoint
       { keyMapFileKeyMap =
             combineFileKeymap
                 (keyMapFileKeyMap startingPoint)
                 (confFileKeyConfig kbc)
       , keyMapReportsKeyMap =
             combineReportsKeymap
                 (keyMapReportsKeyMap startingPoint)
                 (confReportsKeyConfig kbc)
       , keyMapHelpMatchers =
             combineKeyMappings
                 (keyMapHelpMatchers startingPoint)
                 (confHelpKeyConfig kbc)
       }

combineFileKeymap :: FileKeyMap -> Maybe FileKeyConfigs -> FileKeyMap
combineFileKeymap fkm Nothing = fkm
combineFileKeymap fkm (Just fkc) =
    fkm
    { fileKeyMapEmptyMatchers =
          combineKeyMappings (fileKeyMapEmptyMatchers fkm) (emptyKeyConfigs fkc)
    , fileKeyMapEntryMatchers =
          combineKeyMappings (fileKeyMapEntryMatchers fkm) (entryKeyConfigs fkc)
    , fileKeyMapHeaderMatchers =
          combineKeyMappings
              (fileKeyMapHeaderMatchers fkm)
              (headerKeyConfigs fkc)
    , fileKeyMapContentsMatchers =
          combineKeyMappings
              (fileKeyMapContentsMatchers fkm)
              (contentsKeyConfigs fkc)
    , fileKeyMapTimestampsMatchers =
          combineKeyMappings
              (fileKeyMapTimestampsMatchers fkm)
              (timestampsKeyConfigs fkc)
    , fileKeyMapPropertiesMatchers =
          combineKeyMappings
              (fileKeyMapPropertiesMatchers fkm)
              (propertiesKeyConfigs fkc)
    , fileKeyMapStateHistoryMatchers =
          combineKeyMappings
              (fileKeyMapStateHistoryMatchers fkm)
              (stateHistoryKeyConfigs fkc)
    , fileKeyMapTagsMatchers =
          combineKeyMappings (fileKeyMapTagsMatchers fkm) (tagsKeyConfigs fkc)
    , fileKeyMapLogbookMatchers =
          combineKeyMappings
              (fileKeyMapLogbookMatchers fkm)
              (logbookKeyConfigs fkc)
    , fileKeyMapAnyMatchers =
          combineKeyMappings (fileKeyMapAnyMatchers fkm) (anyKeyConfigs fkc)
    }

combineReportsKeymap ::
       ReportsKeyMap -> Maybe ReportsKeyConfigs -> ReportsKeyMap
combineReportsKeymap rkm Nothing = rkm
combineReportsKeymap rkm (Just rkc) =
    rkm
    { reportsKeymapNextActionReportMatchers =
          combineKeyMappings
              (reportsKeymapNextActionReportMatchers rkm)
              (nextActionReportKeyConfigs rkc)
    }

combineKeyMappings :: KeyMappings -> Maybe KeyConfigs -> KeyMappings
combineKeyMappings kms Nothing = kms
combineKeyMappings kms (Just kcs) = map go (keyConfigs kcs) ++ kms
  where
    go :: KeyConfig -> KeyMapping
    go KeyConfig {..} =
        case keyConfigMatcher of
            MatchConfKeyPress kp ->
                MapVtyExactly kp $
                case findAction keyConfigAction of
                    Just (PlainAction a) -> a
                    _ -> error "TODO deal with this error correctly"
            MatchConfCatchAll ->
                MapCatchAll $
                case findAction keyConfigAction of
                    Just (PlainAction a) -> a
                    _ -> error "TODO deal with this error correctly"
            MatchConfAnyChar ->
                MapAnyTypeableChar $
                case findAction keyConfigAction of
                    Just (UsingCharAction a) -> a
                    _ -> error "TODO deal with this error correctly"
    findAction :: ActionName -> Maybe AnyAction
    findAction an = find ((== an) . anyActionName) allActions

getConfiguration :: Arguments -> Environment -> IO (Maybe Configuration)
getConfiguration (Arguments _ Flags {..}) Environment {..} =
    Report.getConfigurationWith [flagConfigFile, envConfigFile]

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    reportEnv <- Report.getEnv
    let getSmosEnv :: String -> Maybe String
        getSmosEnv key = ("SMOS_" ++ key) `lookup` env
    pure
        Environment
        { envConfigFile =
              getSmosEnv "CONFIGURATION_FILE" <|> getSmosEnv "CONFIG_FILE"
        , envReportEnv = reportEnv
        }

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> getArgs >>= handleParseResult

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
    description = "Smos editor"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> editParser <*> parseFlags

parseFlags :: Parser Flags
parseFlags = Flags <$> parseConfigFileFlag <*> Report.parseFlags

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILEPATH"
             , help "The configuration file to use"
             , value Nothing
             ])
