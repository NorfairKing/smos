{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.OptParse
  ( getInstructions,
    Instructions (..),
    runArgumentsParser,
  )
where

import qualified Data.Text as T
import Import
import Options.Applicative
import Smos.Actions
import Smos.Keys
import Smos.OptParse.Bare
import Smos.OptParse.Types
import qualified Smos.Report.OptParse as Report
import qualified Smos.Report.OptParse.Types as Report
import Smos.Types
import qualified System.Environment as System
import System.Exit (die)
import YamlParse.Applicative (confDesc)

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
  Arguments fp flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions conf fp (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions ::
  SmosConfig -> FilePath -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions sc@SmosConfig {..} fp Flags {..} Environment {..} mc = do
  p <- resolveFile' fp
  src <-
    Report.combineToConfig
      configReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConf <$> mc)
  keyMap <-
    case combineKeymap configKeyMap $ mc >>= confKeybindingsConf of
      CombErr errs -> die $ unlines $ map prettyCombError errs
      Combined keyMap -> pure keyMap
  let sc' = sc {configKeyMap = keyMap, configReportConfig = src}
  pure $ Instructions p sc'

combineKeymap :: KeyMap -> Maybe KeybindingsConfiguration -> Comb KeyMap
combineKeymap km Nothing = pure km
combineKeymap km (Just kbc) = do
  let startingPoint =
        case confReset kbc of
          Just True -> mempty
          Just False -> km
          Nothing -> km
  keyMapFileKeyMap <- combineFileKeymap (keyMapFileKeyMap startingPoint) (confFileKeyConfig kbc)
  keyMapReportsKeyMap <-
    combineReportsKeymap (keyMapReportsKeyMap startingPoint) (confReportsKeyConfig kbc)
  keyMapHelpKeyMap <- combineHelpKeymap (keyMapHelpKeyMap startingPoint) (confHelpKeyConfig kbc)
  return
    startingPoint
      { keyMapFileKeyMap = keyMapFileKeyMap,
        keyMapReportsKeyMap = keyMapReportsKeyMap,
        keyMapHelpKeyMap = keyMapHelpKeyMap
      }

combineFileKeymap :: FileKeyMap -> Maybe FileKeyConfigs -> Comb FileKeyMap
combineFileKeymap fkm Nothing = pure fkm
combineFileKeymap fkm (Just fkc) = do
  fileKeyMapEmptyMatchers <- combineKeyMappings (fileKeyMapEmptyMatchers fkm) (emptyKeyConfigs fkc)
  fileKeyMapEntryMatchers <- combineKeyMappings (fileKeyMapEntryMatchers fkm) (entryKeyConfigs fkc)
  fileKeyMapHeaderMatchers <-
    combineKeyMappings (fileKeyMapHeaderMatchers fkm) (headerKeyConfigs fkc)
  fileKeyMapContentsMatchers <-
    combineKeyMappings (fileKeyMapContentsMatchers fkm) (contentsKeyConfigs fkc)
  fileKeyMapTimestampsMatchers <-
    combineKeyMappings (fileKeyMapTimestampsMatchers fkm) (timestampsKeyConfigs fkc)
  fileKeyMapPropertiesMatchers <-
    combineKeyMappings (fileKeyMapPropertiesMatchers fkm) (propertiesKeyConfigs fkc)
  fileKeyMapStateHistoryMatchers <-
    combineKeyMappings (fileKeyMapStateHistoryMatchers fkm) (stateHistoryKeyConfigs fkc)
  fileKeyMapTagsMatchers <- combineKeyMappings (fileKeyMapTagsMatchers fkm) (tagsKeyConfigs fkc)
  fileKeyMapLogbookMatchers <-
    combineKeyMappings (fileKeyMapLogbookMatchers fkm) (logbookKeyConfigs fkc)
  fileKeyMapAnyMatchers <- combineKeyMappings (fileKeyMapAnyMatchers fkm) (anyKeyConfigs fkc)
  return $
    fkm
      { fileKeyMapEmptyMatchers = fileKeyMapEmptyMatchers,
        fileKeyMapEntryMatchers = fileKeyMapEntryMatchers,
        fileKeyMapHeaderMatchers = fileKeyMapHeaderMatchers,
        fileKeyMapContentsMatchers = fileKeyMapContentsMatchers,
        fileKeyMapTimestampsMatchers = fileKeyMapTimestampsMatchers,
        fileKeyMapPropertiesMatchers = fileKeyMapPropertiesMatchers,
        fileKeyMapStateHistoryMatchers = fileKeyMapStateHistoryMatchers,
        fileKeyMapTagsMatchers = fileKeyMapTagsMatchers,
        fileKeyMapLogbookMatchers = fileKeyMapLogbookMatchers,
        fileKeyMapAnyMatchers = fileKeyMapAnyMatchers
      }

combineReportsKeymap :: ReportsKeyMap -> Maybe ReportsKeyConfigs -> Comb ReportsKeyMap
combineReportsKeymap rkm Nothing = pure rkm
combineReportsKeymap rkm (Just rkc) = do
  nams <-
    combineKeyMappings (reportsKeymapNextActionReportMatchers rkm) (nextActionReportKeyConfigs rkc)
  return $ rkm {reportsKeymapNextActionReportMatchers = nams}

combineHelpKeymap :: HelpKeyMap -> Maybe HelpKeyConfigs -> Comb HelpKeyMap
combineHelpKeymap hkm Nothing = pure hkm
combineHelpKeymap hkm (Just hkc) = do
  hms <- combineKeyMappings (helpKeyMapHelpMatchers hkm) (helpHelpKeyConfigs hkc)
  sms <- combineKeyMappings (helpKeyMapSearchMatchers hkm) (helpSearchKeyConfigs hkc)
  return $ hkm {helpKeyMapHelpMatchers = hms, helpKeyMapSearchMatchers = sms}

combineKeyMappings :: KeyMappings -> Maybe KeyConfigs -> Comb KeyMappings
combineKeyMappings kms Nothing = pure kms
combineKeyMappings kms (Just kcs) = (++ kms) <$> traverse go (keyConfigs kcs)
  where
    go :: KeyConfig -> Comb KeyMapping
    go KeyConfig {..} =
      case keyConfigMatcher of
        MatchConfKeyPress kp ->
          MapVtyExactly kp
            <$> case findAction keyConfigAction of
              Just (PlainAction a) -> pure a
              Just _ -> CombErr [ActionWrongType keyConfigAction]
              Nothing -> CombErr [ActionNotFound keyConfigAction]
        MatchConfCatchAll ->
          MapCatchAll
            <$> case findAction keyConfigAction of
              Just (PlainAction a) -> pure a
              Just _ -> CombErr [ActionWrongType keyConfigAction]
              Nothing -> CombErr [ActionNotFound keyConfigAction]
        MatchConfAnyChar ->
          MapAnyTypeableChar
            <$> case findAction keyConfigAction of
              Just (UsingCharAction a) -> pure a
              Just _ -> CombErr [ActionWrongType keyConfigAction]
              Nothing -> CombErr [ActionNotFound keyConfigAction]
        MatchConfCombination kp mc ->
          let go' :: MatcherConfig -> Comb KeyMapping
              go' mc_ =
                case mc_ of
                  MatchConfKeyPress kp_ ->
                    MapVtyExactly kp_
                      <$> case findAction keyConfigAction of
                        Just (PlainAction a) -> pure a
                        Just _ -> CombErr [ActionWrongType keyConfigAction]
                        Nothing -> CombErr [ActionNotFound keyConfigAction]
                  MatchConfAnyChar ->
                    MapAnyTypeableChar
                      <$> case findAction keyConfigAction of
                        Just (UsingCharAction a) -> pure a
                        Just _ -> CombErr [ActionWrongType keyConfigAction]
                        Nothing -> CombErr [ActionNotFound keyConfigAction]
                  MatchConfCatchAll ->
                    MapCatchAll
                      <$> case findAction keyConfigAction of
                        Just (PlainAction a) -> pure a
                        Just _ -> CombErr [ActionWrongType keyConfigAction]
                        Nothing -> CombErr [ActionNotFound keyConfigAction]
                  MatchConfCombination kp_ mc__ -> MapCombination kp_ <$> go' mc__
           in MapCombination kp <$> go' mc
    findAction :: ActionName -> Maybe AnyAction
    findAction an = find ((== an) . anyActionName) allActions

prettyCombError :: CombineError -> String
prettyCombError (ActionNotFound a) = unwords ["Action not found:", T.unpack $ actionNameText a]
prettyCombError (ActionWrongType a) =
  unwords ["Action found, but of the wrong type:", T.unpack $ actionNameText a]

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Report.getEnvWithConfigFile $ Environment <$> Report.getEnvironment

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> System.getArgs >>= handleParseResult

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
    description = "Smos editor"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> editParser <*> Report.parseFlagsWithConfigFile parseFlags

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseFlags
