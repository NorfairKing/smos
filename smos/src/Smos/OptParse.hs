{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse where

import Autodocodec
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import OptEnvConf
import Paths_smos (version)
import Smos.Actions
import Smos.CLI.OptParse
import Smos.Keys
import qualified Smos.Report.OptParse as Report
import Smos.Types
import System.Exit (die)

getInstructions :: SmosConfig -> IO (Maybe StartingPath, SmosConfig)
getInstructions sc@SmosConfig {..} = do
  Instructions msp Settings {..} <- runSettingsParser version "Smos TUI editor"
  keyMap <-
    case combineKeymap configKeyMap settingKeybindings of
      CombErr errs -> die $ unlines $ map prettyCombError errs
      Combined keyMap -> pure keyMap
  let SmosConfig _ _ _ _ = undefined
  let sc' =
        sc
          { configKeyMap = keyMap,
            configReportSettings = settingReportSettings,
            configExplainerMode = fromMaybe configExplainerMode settingExplainerMode,
            configSandboxMode = fromMaybe configSandboxMode settingSandboxMode
          }
  pure (msp, sc')

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
  ReportsKeyMap
    <$> combineNextActionReportKeyMap (reportsKeymapNextActionReportKeyMap rkm) (nextActionReportKeyConfigs rkc)
    <*> combineWaitingReportKeyMap (reportsKeymapWaitingReportKeyMap rkm) (waitingReportKeyConfigs rkc)
    <*> combineOngoingReportKeyMap (reportsKeymapOngoingReportKeyMap rkm) (ongoingReportKeyConfigs rkc)
    <*> combineTimestampsReportKeyMap (reportsKeymapTimestampsReportKeyMap rkm) (timestampsReportKeyConfigs rkc)
    <*> combineStuckReportKeyMap (reportsKeymapStuckReportKeyMap rkm) (stuckReportKeyConfigs rkc)
    <*> combineWorkReportKeyMap (reportsKeymapWorkReportKeyMap rkm) (workReportKeyConfigs rkc)
    <*> combineKeyMappings (reportsKeymapAnyMatchers rkm) (anyReportKeyConfigs rkc)

combineNextActionReportKeyMap :: NextActionReportKeyMap -> Maybe NextActionReportKeyConfigs -> Comb NextActionReportKeyMap
combineNextActionReportKeyMap narkm Nothing = pure narkm
combineNextActionReportKeyMap narkm (Just narkc) =
  NextActionReportKeyMap
    <$> combineKeyMappings (nextActionReportMatchers narkm) (nextActionReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (nextActionReportSearchMatchers narkm) (nextActionReportSearchKeyConfigs narkc)
    <*> combineKeyMappings (nextActionReportAnyMatchers narkm) (nextActionReportAnyKeyConfigs narkc)

combineWaitingReportKeyMap :: WaitingReportKeyMap -> Maybe WaitingReportKeyConfigs -> Comb WaitingReportKeyMap
combineWaitingReportKeyMap narkm Nothing = pure narkm
combineWaitingReportKeyMap narkm (Just narkc) =
  WaitingReportKeyMap
    <$> combineKeyMappings (waitingReportMatchers narkm) (waitingReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (waitingReportSearchMatchers narkm) (waitingReportSearchKeyConfigs narkc)
    <*> combineKeyMappings (waitingReportAnyMatchers narkm) (waitingReportAnyKeyConfigs narkc)

combineOngoingReportKeyMap :: OngoingReportKeyMap -> Maybe OngoingReportKeyConfigs -> Comb OngoingReportKeyMap
combineOngoingReportKeyMap narkm Nothing = pure narkm
combineOngoingReportKeyMap narkm (Just narkc) =
  OngoingReportKeyMap
    <$> combineKeyMappings (ongoingReportMatchers narkm) (ongoingReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (ongoingReportSearchMatchers narkm) (ongoingReportSearchKeyConfigs narkc)
    <*> combineKeyMappings (ongoingReportAnyMatchers narkm) (ongoingReportAnyKeyConfigs narkc)

combineTimestampsReportKeyMap :: TimestampsReportKeyMap -> Maybe TimestampsReportKeyConfigs -> Comb TimestampsReportKeyMap
combineTimestampsReportKeyMap narkm Nothing = pure narkm
combineTimestampsReportKeyMap narkm (Just narkc) =
  TimestampsReportKeyMap
    <$> combineKeyMappings (timestampsReportMatchers narkm) (timestampsReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (timestampsReportSearchMatchers narkm) (timestampsReportSearchKeyConfigs narkc)
    <*> combineKeyMappings (timestampsReportAnyMatchers narkm) (timestampsReportAnyKeyConfigs narkc)

combineStuckReportKeyMap :: StuckReportKeyMap -> Maybe StuckReportKeyConfigs -> Comb StuckReportKeyMap
combineStuckReportKeyMap narkm Nothing = pure narkm
combineStuckReportKeyMap narkm (Just narkc) =
  StuckReportKeyMap
    <$> combineKeyMappings (stuckReportMatchers narkm) (stuckReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (stuckReportAnyMatchers narkm) (stuckReportAnyKeyConfigs narkc)

combineWorkReportKeyMap :: WorkReportKeyMap -> Maybe WorkReportKeyConfigs -> Comb WorkReportKeyMap
combineWorkReportKeyMap narkm Nothing = pure narkm
combineWorkReportKeyMap narkm (Just narkc) =
  WorkReportKeyMap
    <$> combineKeyMappings (workReportMatchers narkm) (workReportNormalKeyConfigs narkc)
    <*> combineKeyMappings (workReportSearchMatchers narkm) (workReportSearchKeyConfigs narkc)
    <*> combineKeyMappings (workReportAnyMatchers narkm) (workReportAnyKeyConfigs narkc)

combineHelpKeymap :: HelpKeyMap -> Maybe HelpKeyConfigs -> Comb HelpKeyMap
combineHelpKeymap hkm Nothing = pure hkm
combineHelpKeymap hkm (Just hkc) =
  HelpKeyMap
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

data KeybindingsConfiguration = KeybindingsConfiguration
  { confReset :: !(Maybe Bool),
    confFileKeyConfig :: !(Maybe FileKeyConfigs),
    confBrowserKeyConfig :: !(Maybe BrowserKeyConfigs),
    confReportsKeyConfig :: !(Maybe ReportsKeyConfigs),
    confHelpKeyConfig :: !(Maybe HelpKeyConfigs),
    confAnyKeyConfig :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity KeybindingsConfiguration

instance HasParser KeybindingsConfiguration where
  settingsParser = parseKeybindingsConfiguration

{-# ANN parseKeybindingsConfiguration ("NOCOVER" :: String) #-}
parseKeybindingsConfiguration :: OptEnvConf.Parser KeybindingsConfiguration
parseKeybindingsConfiguration = do
  confReset <-
    setting
      [ help "Whether to reset all keybindings. Set this to false to add keys, set this to true to replace keys.",
        conf "reset"
      ]
  confFileKeyConfig <-
    setting
      [ help "Keybindings for the file context",
        conf "file"
      ]
  confBrowserKeyConfig <-
    setting
      [ help "Keybindings for the file browser context",
        conf "browser"
      ]
  confReportsKeyConfig <-
    setting
      [ help "Keybindings for the reports context",
        conf "reports"
      ]
  confHelpKeyConfig <-
    setting
      [ help "Keybindings for the help context",
        conf "help"
      ]
  confAnyKeyConfig <-
    setting
      [ help "Keybindings for any context",
        conf "any"
      ]
  pure KeybindingsConfiguration {..}

data FileKeyConfigs = FileKeyConfigs
  { emptyKeyConfigs :: !(Maybe KeyConfigs),
    entryKeyConfigs :: !(Maybe KeyConfigs),
    headerKeyConfigs :: !(Maybe KeyConfigs),
    contentsKeyConfigs :: !(Maybe KeyConfigs),
    timestampsKeyConfigs :: !(Maybe KeyConfigs),
    propertiesKeyConfigs :: !(Maybe KeyConfigs),
    stateHistoryKeyConfigs :: !(Maybe KeyConfigs),
    tagsKeyConfigs :: !(Maybe KeyConfigs),
    logbookKeyConfigs :: !(Maybe KeyConfigs),
    anyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity FileKeyConfigs

instance HasCodec FileKeyConfigs where
  codec =
    object "FileKeyConfigs" $
      FileKeyConfigs
        <$> optionalFieldOrNull "empty" "Keybindings for when the file is empty"
          .= emptyKeyConfigs
        <*> optionalFieldOrNull "entry" "Keybindings for when an entry is selected"
          .= entryKeyConfigs
        <*> optionalFieldOrNull "header" "Keybindings for when an header is selected"
          .= headerKeyConfigs
        <*> optionalFieldOrNull "contents" "Keybindings for when an contents is selected"
          .= contentsKeyConfigs
        <*> optionalFieldOrNull "timestamps" "Keybindings for when a timestamps are selected"
          .= timestampsKeyConfigs
        <*> optionalFieldOrNull "properties" "Keybindings for when a properties are selected"
          .= propertiesKeyConfigs
        <*> optionalFieldOrNull "state-history" "Keybindings for when a state history is selected"
          .= stateHistoryKeyConfigs
        <*> optionalFieldOrNull "tags" "Keybindings for when a tags are selected"
          .= tagsKeyConfigs
        <*> optionalFieldOrNull "logbook" "Keybindings for when a logbook is selected"
          .= logbookKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings that match in any file subcontext"
          .= anyKeyConfigs

data BrowserKeyConfigs = BrowserKeyConfigs
  { browserExistentKeyConfigs :: Maybe KeyConfigs,
    browserInProgressKeyConfigs :: Maybe KeyConfigs,
    browserEmptyKeyConfigs :: Maybe KeyConfigs,
    browserFilterKeyConfigs :: Maybe KeyConfigs,
    browserAnyKeyConfigs :: Maybe KeyConfigs
  }
  deriving stock (Show, Generic)

instance Validity BrowserKeyConfigs

instance HasCodec BrowserKeyConfigs where
  codec =
    object "BrowserKeyConfigs" $
      BrowserKeyConfigs
        <$> optionalFieldOrNull "existent" "Keybindings for when an existing file or directory is selected"
          .= browserExistentKeyConfigs
        <*> optionalFieldOrNull "in-progress" "Keybindings for when an in-progress file or directory is selected"
          .= browserInProgressKeyConfigs
        <*> optionalFieldOrNull "empty" "Keybindings for when the directory being browsed is empty"
          .= browserEmptyKeyConfigs
        <*> optionalFieldOrNull "filter" "Keybindings for when file browser's filter bar is selected"
          .= browserFilterKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for any of the other file browser situations"
          .= browserAnyKeyConfigs

data ReportsKeyConfigs = ReportsKeyConfigs
  { nextActionReportKeyConfigs :: Maybe NextActionReportKeyConfigs,
    waitingReportKeyConfigs :: Maybe WaitingReportKeyConfigs,
    ongoingReportKeyConfigs :: Maybe OngoingReportKeyConfigs,
    timestampsReportKeyConfigs :: Maybe TimestampsReportKeyConfigs,
    stuckReportKeyConfigs :: Maybe StuckReportKeyConfigs,
    workReportKeyConfigs :: Maybe WorkReportKeyConfigs,
    anyReportKeyConfigs :: Maybe KeyConfigs
  }
  deriving stock (Show, Generic)

instance Validity ReportsKeyConfigs

instance HasCodec ReportsKeyConfigs where
  codec =
    object "ReportsKeyConfigs" $
      ReportsKeyConfigs
        <$> optionalFieldOrNull "next-action" "Keybindings for the interactive next action report"
          .= nextActionReportKeyConfigs
        <*> optionalFieldOrNull "waiting" "Keybindings for the interactive waiting report"
          .= waitingReportKeyConfigs
        <*> optionalFieldOrNull "ongoing" "Keybindings for the interactive ongoing report"
          .= ongoingReportKeyConfigs
        <*> optionalFieldOrNull "timestamps" "Keybindings for the interactive timestamps report"
          .= timestampsReportKeyConfigs
        <*> optionalFieldOrNull "stuck" "Keybindings for the interactive stuck projects report"
          .= stuckReportKeyConfigs
        <*> optionalFieldOrNull "work" "Keybindings for the interactive work report"
          .= workReportKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in any report"
          .= anyReportKeyConfigs

data NextActionReportKeyConfigs = NextActionReportKeyConfigs
  { nextActionReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    nextActionReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    nextActionReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity NextActionReportKeyConfigs

instance HasCodec NextActionReportKeyConfigs where
  codec =
    object "NextActionReportKeyConfigs" $
      NextActionReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the next-action report"
          .= nextActionReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the next-action report"
          .= nextActionReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the next-action report"
          .= nextActionReportAnyKeyConfigs

data WaitingReportKeyConfigs = WaitingReportKeyConfigs
  { waitingReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    waitingReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    waitingReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity WaitingReportKeyConfigs

instance HasCodec WaitingReportKeyConfigs where
  codec =
    object "WaitingReportKeyConfigs" $
      WaitingReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the waiting report"
          .= waitingReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the waiting report"
          .= waitingReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the waiting report"
          .= waitingReportAnyKeyConfigs

data OngoingReportKeyConfigs = OngoingReportKeyConfigs
  { ongoingReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    ongoingReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    ongoingReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity OngoingReportKeyConfigs

instance HasCodec OngoingReportKeyConfigs where
  codec =
    object "OngoingReportKeyConfigs" $
      OngoingReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the ongoing report"
          .= ongoingReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the ongoing report"
          .= ongoingReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the ongoing report"
          .= ongoingReportAnyKeyConfigs

data TimestampsReportKeyConfigs = TimestampsReportKeyConfigs
  { timestampsReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    timestampsReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    timestampsReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Generic)

instance Validity TimestampsReportKeyConfigs

instance HasCodec TimestampsReportKeyConfigs where
  codec =
    object "TimestampsReportKeyConfigs" $
      TimestampsReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the timestamps report"
          .= timestampsReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the timestamps report"
          .= timestampsReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the timestamps report"
          .= timestampsReportAnyKeyConfigs

data StuckReportKeyConfigs = StuckReportKeyConfigs
  { stuckReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    stuckReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Generic)

instance Validity StuckReportKeyConfigs

instance HasCodec StuckReportKeyConfigs where
  codec =
    object "StuckReportKeyConfigs" $
      StuckReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the stuck report"
          .= stuckReportNormalKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the stuck report"
          .= stuckReportAnyKeyConfigs

data WorkReportKeyConfigs = WorkReportKeyConfigs
  { workReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    workReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    workReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Generic)

instance Validity WorkReportKeyConfigs

instance HasCodec WorkReportKeyConfigs where
  codec =
    object "WorkReportKeyConfigs" $
      WorkReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the work report"
          .= workReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the work report"
          .= workReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the work report"
          .= workReportAnyKeyConfigs

data HelpKeyConfigs = HelpKeyConfigs
  { helpHelpKeyConfigs :: !(Maybe KeyConfigs),
    helpSearchKeyConfigs :: !(Maybe KeyConfigs),
    helpAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Generic)

instance Validity HelpKeyConfigs

instance HasCodec HelpKeyConfigs where
  codec =
    object "HelpKeyConfigs" $
      HelpKeyConfigs
        <$> optionalFieldOrNull "help" "Keybindings for when in the help screen"
          .= helpHelpKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for when the search bar is selected within the help screen"
          .= helpSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any time in the help screen"
          .= helpAnyKeyConfigs

newtype KeyConfigs = KeyConfigs
  { keyConfigs :: [KeyConfig]
  }
  deriving stock (Show, Generic)
  deriving newtype (Validity)

instance HasCodec KeyConfigs where
  codec = named "KeyConfigs" $ dimapCodec KeyConfigs keyConfigs codec

data KeyConfig = KeyConfig
  { keyConfigMatcher :: !MatcherConfig,
    keyConfigAction :: !ActionName
  }
  deriving (Show, Generic)

instance Validity KeyConfig

instance HasCodec KeyConfig where
  codec =
    named "KeyConfig" $
      object "KeyConfig" $
        KeyConfig
          <$> requiredField "key" "The key to match"
            .= keyConfigMatcher
          <*> requiredField "action" "The name of the action to perform when the key is matched"
            .= keyConfigAction

data Settings = Settings
  { settingKeybindings :: Maybe KeybindingsConfiguration,
    settingReportSettings :: !Report.ReportSettings,
    settingExplainerMode :: !(Maybe Bool),
    settingSandboxMode :: !(Maybe Bool)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  settingKeybindings <- optional settingsParser
  settingReportSettings <- settingsParser
  settingExplainerMode <-
    optional $
      setting
        [ help "Activate explainer mode to show what is happening",
          switch True,
          value False,
          reader exists,
          long "explainer-mode",
          conf "explainer-mode",
          env "EXPLAINER_MODE",
          metavar "ANY"
        ]
  settingSandboxMode <-
    optional $
      setting
        [ help "Activate sandbox mode to ensure that smos can only edit smos files",
          switch True,
          value False,
          reader exists,
          long "explainer-mode",
          conf "explainer-mode",
          env "EXPLAINER_MODE",
          metavar "ANY"
        ]

  pure Settings {..}

data Instructions
  = Instructions (Maybe StartingPath) Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> optional settingsParser
        <*> settingsParser

data CombineError
  = ActionNotFound ActionName
  | ActionWrongType ActionName

data Comb a
  = Combined a
  | CombErr [CombineError]

instance Functor Comb where
  fmap f (Combined a) = Combined (f a)
  fmap _ (CombErr errs) = CombErr errs

instance Applicative Comb where
  pure = Combined
  cfa <*> ca =
    case (cfa, ca) of
      (Combined f, Combined a) -> Combined (f a)
      (CombErr err1, CombErr err2) -> CombErr $ err1 ++ err2
      (CombErr err1, _) -> CombErr err1
      (_, CombErr err2) -> CombErr err2
