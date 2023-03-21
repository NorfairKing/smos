{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse
  ( module Smos.OptParse,
    module Smos.OptParse.Types,
  )
where

import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos
import Smos.Actions
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Keys
import Smos.OptParse.Bare
import Smos.OptParse.Types
import qualified Smos.Report.OptParse as Report
import Smos.Types
import qualified System.Environment as System
import System.Exit (die)

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
  Arguments mfp flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions conf mfp (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions ::
  SmosConfig -> Maybe FilePath -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions sc@SmosConfig {..} mfp Flags {..} Environment {..} mc = do
  curDir <- getCurrentDir
  mst <- mapM (resolveStartingPath curDir) mfp
  src <-
    Report.combineToSettings
      configReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConf <$> mc)
  keyMap <-
    case combineKeymap configKeyMap $ mc >>= confKeybindingsConf of
      CombErr errs -> die $ unlines $ map prettyCombError errs
      Combined keyMap -> pure keyMap
  let SmosConfig _ _ _ _ = undefined
  let sc' =
        sc
          { configKeyMap = keyMap,
            configReportConfig = src,
            configExplainerMode = fromMaybe configExplainerMode $ flagExplainerMode <|> envExplainerMode <|> (mc >>= confExplainerMode),
            configSandboxMode = fromMaybe configSandboxMode $ flagSandboxMode <|> envSandboxMode <|> (mc >>= confSandboxMode)
          }
  pure $ Instructions mst sc'

combineKeymap :: KeyMap -> Maybe KeybindingsConfiguration -> Comb KeyMap
combineKeymap km Nothing = pure km
combineKeymap km (Just kbc) =
  let KeyMap _ _ _ _ _ = undefined
      startingPoint =
        case confReset kbc of
          Just True -> mempty
          Just False -> km
          Nothing -> km
   in KeyMap
        <$> combineFileKeymap (keyMapFileKeyMap startingPoint) (confFileKeyConfig kbc)
        <*> combineBrowserKeyMap (keyMapBrowserKeyMap startingPoint) (confBrowserKeyConfig kbc)
        <*> combineReportsKeymap (keyMapReportsKeyMap startingPoint) (confReportsKeyConfig kbc)
        <*> combineHelpKeymap (keyMapHelpKeyMap startingPoint) (confHelpKeyConfig kbc)
        <*> combineKeyMappings (keyMapAnyKeyMap startingPoint) (confAnyKeyConfig kbc)

combineFileKeymap :: FileKeyMap -> Maybe FileKeyConfigs -> Comb FileKeyMap
combineFileKeymap fkm Nothing = pure fkm
combineFileKeymap fkm (Just fkc) =
  let FileKeyMap _ _ _ _ _ _ _ _ _ _ = undefined
   in FileKeyMap
        <$> combineKeyMappings (fileKeyMapEmptyMatchers fkm) (emptyKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapEntryMatchers fkm) (entryKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapHeaderMatchers fkm) (headerKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapContentsMatchers fkm) (contentsKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapTimestampsMatchers fkm) (timestampsKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapPropertiesMatchers fkm) (propertiesKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapStateHistoryMatchers fkm) (stateHistoryKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapTagsMatchers fkm) (tagsKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapLogbookMatchers fkm) (logbookKeyConfigs fkc)
        <*> combineKeyMappings (fileKeyMapAnyMatchers fkm) (anyKeyConfigs fkc)

combineBrowserKeyMap :: BrowserKeyMap -> Maybe BrowserKeyConfigs -> Comb BrowserKeyMap
combineBrowserKeyMap bkm Nothing = pure bkm
combineBrowserKeyMap bkm (Just bkc) =
  let BrowserKeyMap _ _ _ _ _ = undefined
   in BrowserKeyMap
        <$> combineKeyMappings (browserKeyMapExistentMatchers bkm) (browserExistentKeyConfigs bkc)
        <*> combineKeyMappings (browserKeyMapInProgressMatchers bkm) (browserInProgressKeyConfigs bkc)
        <*> combineKeyMappings (browserKeyMapEmptyMatchers bkm) (browserEmptyKeyConfigs bkc)
        <*> combineKeyMappings (browserKeyMapFilterMatchers bkm) (browserFilterKeyConfigs bkc)
        <*> combineKeyMappings (browserKeyMapAnyMatchers bkm) (browserAnyKeyConfigs bkc)

combineReportsKeymap :: ReportsKeyMap -> Maybe ReportsKeyConfigs -> Comb ReportsKeyMap
combineReportsKeymap rkm Nothing = pure rkm
combineReportsKeymap rkm (Just rkc) =
  let ReportsKeyMap _ _ _ _ _ _ = undefined
   in ReportsKeyMap
        <$> combineNextActionReportKeyMap (reportsKeymapNextActionReportKeyMap rkm) (nextActionReportKeyConfigs rkc)
        <*> combineWaitingReportKeyMap (reportsKeymapWaitingReportKeyMap rkm) (waitingReportKeyConfigs rkc)
        <*> combineTimestampsReportKeyMap (reportsKeymapTimestampsReportKeyMap rkm) (timestampsReportKeyConfigs rkc)
        <*> combineStuckReportKeyMap (reportsKeymapStuckReportKeyMap rkm) (stuckReportKeyConfigs rkc)
        <*> combineWorkReportKeyMap (reportsKeymapWorkReportKeyMap rkm) (workReportKeyConfigs rkc)
        <*> combineKeyMappings (reportsKeymapAnyMatchers rkm) (anyReportKeyConfigs rkc)

combineNextActionReportKeyMap :: NextActionReportKeyMap -> Maybe NextActionReportKeyConfigs -> Comb NextActionReportKeyMap
combineNextActionReportKeyMap narkm Nothing = pure narkm
combineNextActionReportKeyMap narkm (Just narkc) =
  let NextActionReportKeyMap _ _ _ = undefined
   in NextActionReportKeyMap
        <$> combineKeyMappings (nextActionReportMatchers narkm) (nextActionReportNormalKeyConfigs narkc)
        <*> combineKeyMappings (nextActionReportSearchMatchers narkm) (nextActionReportSearchKeyConfigs narkc)
        <*> combineKeyMappings (nextActionReportAnyMatchers narkm) (nextActionReportAnyKeyConfigs narkc)

combineWaitingReportKeyMap :: WaitingReportKeyMap -> Maybe WaitingReportKeyConfigs -> Comb WaitingReportKeyMap
combineWaitingReportKeyMap narkm Nothing = pure narkm
combineWaitingReportKeyMap narkm (Just narkc) =
  let WaitingReportKeyMap _ _ _ = undefined
   in WaitingReportKeyMap
        <$> combineKeyMappings (waitingReportMatchers narkm) (waitingReportNormalKeyConfigs narkc)
        <*> combineKeyMappings (waitingReportSearchMatchers narkm) (waitingReportSearchKeyConfigs narkc)
        <*> combineKeyMappings (waitingReportAnyMatchers narkm) (waitingReportAnyKeyConfigs narkc)

combineTimestampsReportKeyMap :: TimestampsReportKeyMap -> Maybe TimestampsReportKeyConfigs -> Comb TimestampsReportKeyMap
combineTimestampsReportKeyMap narkm Nothing = pure narkm
combineTimestampsReportKeyMap narkm (Just narkc) =
  let TimestampsReportKeyMap _ _ _ = undefined
   in TimestampsReportKeyMap
        <$> combineKeyMappings (timestampsReportMatchers narkm) (timestampsReportNormalKeyConfigs narkc)
        <*> combineKeyMappings (timestampsReportSearchMatchers narkm) (timestampsReportSearchKeyConfigs narkc)
        <*> combineKeyMappings (timestampsReportAnyMatchers narkm) (timestampsReportAnyKeyConfigs narkc)

combineStuckReportKeyMap :: StuckReportKeyMap -> Maybe StuckReportKeyConfigs -> Comb StuckReportKeyMap
combineStuckReportKeyMap narkm Nothing = pure narkm
combineStuckReportKeyMap narkm (Just narkc) =
  let StuckReportKeyMap _ _ = undefined
   in StuckReportKeyMap
        <$> combineKeyMappings (stuckReportMatchers narkm) (stuckReportNormalKeyConfigs narkc)
        <*> combineKeyMappings (stuckReportAnyMatchers narkm) (stuckReportAnyKeyConfigs narkc)

combineWorkReportKeyMap :: WorkReportKeyMap -> Maybe WorkReportKeyConfigs -> Comb WorkReportKeyMap
combineWorkReportKeyMap narkm Nothing = pure narkm
combineWorkReportKeyMap narkm (Just narkc) =
  let WorkReportKeyMap _ _ _ = undefined
   in WorkReportKeyMap
        <$> combineKeyMappings (workReportMatchers narkm) (workReportNormalKeyConfigs narkc)
        <*> combineKeyMappings (workReportSearchMatchers narkm) (workReportSearchKeyConfigs narkc)
        <*> combineKeyMappings (workReportAnyMatchers narkm) (workReportAnyKeyConfigs narkc)

combineHelpKeymap :: HelpKeyMap -> Maybe HelpKeyConfigs -> Comb HelpKeyMap
combineHelpKeymap hkm Nothing = pure hkm
combineHelpKeymap hkm (Just hkc) =
  let HelpKeyMap _ _ _ = undefined
   in HelpKeyMap
        <$> combineKeyMappings (helpKeyMapHelpMatchers hkm) (helpHelpKeyConfigs hkc)
        <*> combineKeyMappings (helpKeyMapSearchMatchers hkm) (helpSearchKeyConfigs hkc)
        <*> combineKeyMappings (helpKeyMapAnyMatchers hkm) (helpAnyKeyConfigs hkc)

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

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> Report.environmentParser
      <*> optional (Env.var Env.auto "EXPLAINER_MODE" (Env.help "Activate explainer mode to show what is happening"))
      <*> optional (Env.var Env.auto "SANDBOX_MODE" (Env.help "Activate sandbox mode to ensure that smos can only edit smos files"))

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> System.getArgs >>= handleParseResult

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos TUI Editor version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> editParser
    <*> parseFlagsWithConfigFile parseFlags

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> Report.parseFlags
    <*> optional (flag' True (mconcat [long "explainer-mode", help "Activate explainer mode to show what is happening"]))
    <*> optional (flag' True (mconcat [long "sandbox-mode", help "Activate sandbox mode to ensure that smos can only edit smos files"]))
