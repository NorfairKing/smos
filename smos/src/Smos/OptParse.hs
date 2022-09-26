{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse
  ( module Smos.OptParse,
    module Smos.OptParse.Types,
  )
where

import qualified Data.Text as T
import Data.Version
import qualified Env
import Import
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Paths_smos
import Smos.Actions
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
  combineToInstructions conf mfp (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions ::
  SmosConfig -> Maybe FilePath -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions sc@SmosConfig {..} mfp Flags {..} Environment {..} mc = do
  curDir <- getCurrentDir
  mst <- mapM (resolveStartingPath curDir) mfp
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
combineKeymap km (Just kbc) = do
  let KeyMap _ _ _ _ _ = undefined
  let startingPoint =
        case confReset kbc of
          Just True -> mempty
          Just False -> km
          Nothing -> km
  keyMapFileKeyMap <- combineFileKeymap (keyMapFileKeyMap startingPoint) (confFileKeyConfig kbc)
  keyMapBrowserKeyMap <- combineBrowserKeyMap (keyMapBrowserKeyMap km) (confBrowserKeyConfig kbc)
  keyMapReportsKeyMap <-
    combineReportsKeymap (keyMapReportsKeyMap startingPoint) (confReportsKeyConfig kbc)
  keyMapHelpKeyMap <- combineHelpKeymap (keyMapHelpKeyMap startingPoint) (confHelpKeyConfig kbc)
  keyMapAnyKeyMap <- combineKeyMappings (keyMapAnyKeyMap km) (confAnyKeyConfig kbc)
  pure
    startingPoint
      { keyMapFileKeyMap = keyMapFileKeyMap,
        keyMapBrowserKeyMap = keyMapBrowserKeyMap,
        keyMapReportsKeyMap = keyMapReportsKeyMap,
        keyMapHelpKeyMap = keyMapHelpKeyMap,
        keyMapAnyKeyMap = keyMapAnyKeyMap
      }

combineFileKeymap :: FileKeyMap -> Maybe FileKeyConfigs -> Comb FileKeyMap
combineFileKeymap fkm Nothing = pure fkm
combineFileKeymap fkm (Just fkc) = do
  let FileKeyMap _ _ _ _ _ _ _ _ _ _ = undefined
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
  pure $
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

combineBrowserKeyMap :: BrowserKeyMap -> Maybe BrowserKeyConfigs -> Comb BrowserKeyMap
combineBrowserKeyMap bkm Nothing = pure bkm
combineBrowserKeyMap bkm (Just bkc) = do
  ekms <- combineKeyMappings (browserKeyMapExistentMatchers bkm) (browserExistentKeyConfigs bkc)
  pms <- combineKeyMappings (browserKeyMapInProgressMatchers bkm) (browserInProgressKeyConfigs bkc)
  ems <- combineKeyMappings (browserKeyMapEmptyMatchers bkm) (browserEmptyKeyConfigs bkc)
  ams <- combineKeyMappings (browserKeyMapAnyMatchers bkm) (browserAnyKeyConfigs bkc)
  pure $
    bkm
      { browserKeyMapExistentMatchers = ekms,
        browserKeyMapInProgressMatchers = pms,
        browserKeyMapEmptyMatchers = ems,
        browserKeyMapAnyMatchers = ams
      }

combineReportsKeymap :: ReportsKeyMap -> Maybe ReportsKeyConfigs -> Comb ReportsKeyMap
combineReportsKeymap rkm Nothing = pure rkm
combineReportsKeymap rkm (Just rkc) = do
  let ReportsKeyMap _ _ _ _ _ _ = undefined
  narkms <- combineNextActionReportKeyMap (reportsKeymapNextActionReportKeyMap rkm) (nextActionReportKeyConfigs rkc)
  wrkms <- combineWaitingReportKeyMap (reportsKeymapWaitingReportKeyMap rkm) (waitingReportKeyConfigs rkc)
  tsrkms <- combineTimestampsReportKeyMap (reportsKeymapTimestampsReportKeyMap rkm) (timestampsReportKeyConfigs rkc)
  srkms <- combineStuckReportKeyMap (reportsKeymapStuckReportKeyMap rkm) (stuckReportKeyConfigs rkc)
  workrkms <- combineWorkReportKeyMap (reportsKeymapWorkReportKeyMap rkm) (workReportKeyConfigs rkc)
  ams <- combineKeyMappings (reportsKeymapAnyMatchers rkm) (anyReportKeyConfigs rkc)
  pure $
    rkm
      { reportsKeymapNextActionReportKeyMap = narkms,
        reportsKeymapWaitingReportKeyMap = wrkms,
        reportsKeymapTimestampsReportKeyMap = tsrkms,
        reportsKeymapStuckReportKeyMap = srkms,
        reportsKeymapWorkReportKeyMap = workrkms,
        reportsKeymapAnyMatchers = ams
      }

combineNextActionReportKeyMap :: NextActionReportKeyMap -> Maybe NextActionReportKeyConfigs -> Comb NextActionReportKeyMap
combineNextActionReportKeyMap narkm Nothing = pure narkm
combineNextActionReportKeyMap narkm (Just narkc) = do
  let NextActionReportKeyMap _ _ _ = undefined
  nms <- combineKeyMappings (nextActionReportMatchers narkm) (nextActionReportNormalKeyConfigs narkc)
  sms <- combineKeyMappings (nextActionReportSearchMatchers narkm) (nextActionReportSearchKeyConfigs narkc)
  ams <- combineKeyMappings (nextActionReportAnyMatchers narkm) (nextActionReportAnyKeyConfigs narkc)
  pure $
    narkm
      { nextActionReportMatchers = nms,
        nextActionReportSearchMatchers = sms,
        nextActionReportAnyMatchers = ams
      }

combineWaitingReportKeyMap :: WaitingReportKeyMap -> Maybe WaitingReportKeyConfigs -> Comb WaitingReportKeyMap
combineWaitingReportKeyMap narkm Nothing = pure narkm
combineWaitingReportKeyMap narkm (Just narkc) = do
  let WaitingReportKeyMap _ _ _ = undefined
  nms <- combineKeyMappings (waitingReportMatchers narkm) (waitingReportNormalKeyConfigs narkc)
  sms <- combineKeyMappings (waitingReportSearchMatchers narkm) (waitingReportSearchKeyConfigs narkc)
  ams <- combineKeyMappings (waitingReportAnyMatchers narkm) (waitingReportAnyKeyConfigs narkc)
  pure $
    narkm
      { waitingReportMatchers = nms,
        waitingReportSearchMatchers = sms,
        waitingReportAnyMatchers = ams
      }

combineTimestampsReportKeyMap :: TimestampsReportKeyMap -> Maybe TimestampsReportKeyConfigs -> Comb TimestampsReportKeyMap
combineTimestampsReportKeyMap narkm Nothing = pure narkm
combineTimestampsReportKeyMap narkm (Just narkc) = do
  let TimestampsReportKeyMap _ _ _ = undefined
  nms <- combineKeyMappings (timestampsReportMatchers narkm) (timestampsReportNormalKeyConfigs narkc)
  sms <- combineKeyMappings (timestampsReportSearchMatchers narkm) (timestampsReportSearchKeyConfigs narkc)
  ams <- combineKeyMappings (timestampsReportAnyMatchers narkm) (timestampsReportAnyKeyConfigs narkc)
  pure $
    narkm
      { timestampsReportMatchers = nms,
        timestampsReportSearchMatchers = sms,
        timestampsReportAnyMatchers = ams
      }

combineStuckReportKeyMap :: StuckReportKeyMap -> Maybe StuckReportKeyConfigs -> Comb StuckReportKeyMap
combineStuckReportKeyMap narkm Nothing = pure narkm
combineStuckReportKeyMap narkm (Just narkc) = do
  let StuckReportKeyMap _ _ = undefined
  nms <- combineKeyMappings (stuckReportMatchers narkm) (stuckReportNormalKeyConfigs narkc)
  ams <- combineKeyMappings (stuckReportAnyMatchers narkm) (stuckReportAnyKeyConfigs narkc)
  pure $
    narkm
      { stuckReportMatchers = nms,
        stuckReportAnyMatchers = ams
      }

combineWorkReportKeyMap :: WorkReportKeyMap -> Maybe WorkReportKeyConfigs -> Comb WorkReportKeyMap
combineWorkReportKeyMap narkm Nothing = pure narkm
combineWorkReportKeyMap narkm (Just narkc) = do
  let WorkReportKeyMap _ _ _ = undefined
  nms <- combineKeyMappings (workReportMatchers narkm) (workReportNormalKeyConfigs narkc)
  sms <- combineKeyMappings (workReportSearchMatchers narkm) (workReportSearchKeyConfigs narkc)
  ams <- combineKeyMappings (workReportAnyMatchers narkm) (workReportAnyKeyConfigs narkc)
  pure $
    narkm
      { workReportMatchers = nms,
        workReportSearchMatchers = sms,
        workReportAnyMatchers = ams
      }

combineHelpKeymap :: HelpKeyMap -> Maybe HelpKeyConfigs -> Comb HelpKeyMap
combineHelpKeymap hkm Nothing = pure hkm
combineHelpKeymap hkm (Just hkc) = do
  let HelpKeyMap _ _ _ = undefined
  hms <- combineKeyMappings (helpKeyMapHelpMatchers hkm) (helpHelpKeyConfigs hkc)
  sms <- combineKeyMappings (helpKeyMapSearchMatchers hkm) (helpSearchKeyConfigs hkc)
  ams <- combineKeyMappings (helpKeyMapAnyMatchers hkm) (helpAnyKeyConfigs hkc)
  pure $
    hkm
      { helpKeyMapHelpMatchers = hms,
        helpKeyMapSearchMatchers = sms,
        helpKeyMapAnyMatchers = ams
      }

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
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment
      <$> Report.environmentParser
      <*> optional (Env.var Env.auto "EXPLAINER_MODE" (Env.help "Activate explainer mode to show what is happening"))
      <*> optional (Env.var Env.auto "SANDBOX_MODE" (Env.help "Activate sandbox mode to ensure that smos can only edit smos files"))

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> System.getArgs >>= handleParseResult

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
          [ "",
            "Smos TUI Editor version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> editParser
    <*> Report.parseFlagsWithConfigFile parseFlags

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> Report.parseFlags
    <*> optional (flag' True (mconcat [long "explainer-mode", help "Activate explainer mode to show what is happening"]))
    <*> optional (flag' True (mconcat [long "sandbox-mode", help "Activate sandbox mode to ensure that smos can only edit smos files"]))
