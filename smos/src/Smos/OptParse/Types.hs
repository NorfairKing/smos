{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Import
import Smos.Keys
import qualified Smos.Report.OptParse.Types as Report
import Smos.Types

data Arguments
  = Arguments (Maybe FilePath) (Report.FlagsWithConfigFile Flags)

data Flags = Flags
  { flagReportFlags :: Report.Flags,
    flagExplainerMode :: Maybe Bool
  }
  deriving (Show, Eq)

data Environment = Environment
  { envReportEnvironment :: Report.Environment,
    envExplainerMode :: Maybe Bool
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confReportConf :: !Report.Configuration,
    confKeybindingsConf :: !(Maybe KeybindingsConfiguration),
    confExplainerMode :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Configuration)

instance Validity Configuration

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> Report.configurationObjectCodec .= confReportConf
        <*> optionalFieldOrNull "keys" "Keybindings" .= confKeybindingsConf
        <*> optionalFieldOrNull "explainer-mode" "Turn on explainer mode where the user can see what is happening" .= confExplainerMode

backToConfiguration :: SmosConfig -> Configuration
backToConfiguration SmosConfig {..} =
  let SmosConfig _ _ _ = undefined
   in Configuration
        { confReportConf = Report.backToConfiguration configReportConfig,
          confKeybindingsConf = Just $ backToKeybindingsConfiguration configKeyMap,
          confExplainerMode = if configExplainerMode then Just True else Nothing
        }

data KeybindingsConfiguration = KeybindingsConfiguration
  { confReset :: !(Maybe Bool),
    confFileKeyConfig :: !(Maybe FileKeyConfigs),
    confBrowserKeyConfig :: !(Maybe BrowserKeyConfigs),
    confReportsKeyConfig :: !(Maybe ReportsKeyConfigs),
    confHelpKeyConfig :: !(Maybe HelpKeyConfigs),
    confAnyKeyConfig :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec KeybindingsConfiguration)

instance Validity KeybindingsConfiguration

instance HasCodec KeybindingsConfiguration where
  codec =
    object "KeybindingsConfiguration" $
      KeybindingsConfiguration
        <$> optionalFieldOrNull "reset" "Whether to reset all keybindings. Set this to false to add keys, set this to true to replace keys." .= confReset
        <*> optionalFieldOrNull "file" "Keybindings for the file context" .= confFileKeyConfig
        <*> optionalFieldOrNull "browser" "Keybindings for the file browser context" .= confBrowserKeyConfig
        <*> optionalFieldOrNull "reports" "Keybindings for the reports context" .= confReportsKeyConfig
        <*> optionalFieldOrNull "help" "Keybindings for the help context" .= confHelpKeyConfig
        <*> optionalFieldOrNull "any" "Keybindings for any context" .= confAnyKeyConfig

backToKeybindingsConfiguration :: KeyMap -> KeybindingsConfiguration
backToKeybindingsConfiguration KeyMap {..} =
  let KeyMap _ _ _ _ _ = undefined
   in KeybindingsConfiguration
        { confReset = Just True,
          confFileKeyConfig = Just $ backToFileKeyConfigs keyMapFileKeyMap,
          confBrowserKeyConfig = Just $ backToBrowserKeyConfigs keyMapBrowserKeyMap,
          confReportsKeyConfig = Just $ backToReportsKeyConfig keyMapReportsKeyMap,
          confHelpKeyConfig = Just $ backToHelpKeyConfigs keyMapHelpKeyMap,
          confAnyKeyConfig = Just $ backToKeyConfigs keyMapAnyKeyMap
        }

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
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec FileKeyConfigs)

instance Validity FileKeyConfigs

instance HasCodec FileKeyConfigs where
  codec =
    object "FileKeyConfigs" $
      FileKeyConfigs
        <$> optionalFieldOrNull "empty" "Keybindings for when the file is empty" .= emptyKeyConfigs
        <*> optionalFieldOrNull "entry" "Keybindings for when an entry is selected" .= entryKeyConfigs
        <*> optionalFieldOrNull "header" "Keybindings for when an header is selected" .= headerKeyConfigs
        <*> optionalFieldOrNull "contents" "Keybindings for when an contents is selected" .= contentsKeyConfigs
        <*> optionalFieldOrNull "timestamps" "Keybindings for when a timestamps are selected" .= timestampsKeyConfigs
        <*> optionalFieldOrNull "properties" "Keybindings for when a properties are selected" .= propertiesKeyConfigs
        <*> optionalFieldOrNull "state-history" "Keybindings for when a state history is selected" .= stateHistoryKeyConfigs
        <*> optionalFieldOrNull "tags" "Keybindings for when a tags are selected" .= tagsKeyConfigs
        <*> optionalFieldOrNull "logbook" "Keybindings for when a logbook is selected" .= logbookKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings that match in any file subcontext" .= anyKeyConfigs

backToFileKeyConfigs :: FileKeyMap -> FileKeyConfigs
backToFileKeyConfigs FileKeyMap {..} =
  let FileKeyMap _ _ _ _ _ _ _ _ _ _ = undefined
   in FileKeyConfigs
        { emptyKeyConfigs = Just $ backToKeyConfigs fileKeyMapEmptyMatchers,
          entryKeyConfigs = Just $ backToKeyConfigs fileKeyMapEntryMatchers,
          headerKeyConfigs = Just $ backToKeyConfigs fileKeyMapHeaderMatchers,
          contentsKeyConfigs = Just $ backToKeyConfigs fileKeyMapContentsMatchers,
          timestampsKeyConfigs = Just $ backToKeyConfigs fileKeyMapTimestampsMatchers,
          propertiesKeyConfigs = Just $ backToKeyConfigs fileKeyMapPropertiesMatchers,
          stateHistoryKeyConfigs = Just $ backToKeyConfigs fileKeyMapStateHistoryMatchers,
          tagsKeyConfigs = Just $ backToKeyConfigs fileKeyMapTagsMatchers,
          logbookKeyConfigs = Just $ backToKeyConfigs fileKeyMapLogbookMatchers,
          anyKeyConfigs = Just $ backToKeyConfigs fileKeyMapAnyMatchers
        }

data BrowserKeyConfigs = BrowserKeyConfigs
  { browserExistentKeyConfigs :: Maybe KeyConfigs,
    browserInProgressKeyConfigs :: Maybe KeyConfigs,
    browserEmptyKeyConfigs :: Maybe KeyConfigs,
    browserFilterKeyConfigs :: Maybe KeyConfigs,
    browserAnyKeyConfigs :: Maybe KeyConfigs
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec BrowserKeyConfigs)

instance Validity BrowserKeyConfigs

instance HasCodec BrowserKeyConfigs where
  codec =
    object "BrowserKeyConfigs" $
      BrowserKeyConfigs
        <$> optionalFieldOrNull "existent" "Keybindings for when an existing file or directory is selected" .= browserExistentKeyConfigs
        <*> optionalFieldOrNull "in-progress" "Keybindings for when an in-progress file or directory is selected" .= browserInProgressKeyConfigs
        <*> optionalFieldOrNull "empty" "Keybindings for when the directory being browsed is empty" .= browserEmptyKeyConfigs
        <*> optionalFieldOrNull "filter" "Keybindings for when file browser's filter bar is selected" .= browserFilterKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for any of the other file browser situations" .= browserAnyKeyConfigs

backToBrowserKeyConfigs :: BrowserKeyMap -> BrowserKeyConfigs
backToBrowserKeyConfigs BrowserKeyMap {..} =
  let BrowserKeyMap _ _ _ _ _ = undefined
   in BrowserKeyConfigs
        { browserExistentKeyConfigs = Just $ backToKeyConfigs browserKeyMapExistentMatchers,
          browserInProgressKeyConfigs = Just $ backToKeyConfigs browserKeyMapInProgressMatchers,
          browserFilterKeyConfigs = Just $ backToKeyConfigs browserKeyMapFilterMatchers,
          browserEmptyKeyConfigs = Just $ backToKeyConfigs browserKeyMapEmptyMatchers,
          browserAnyKeyConfigs = Just $ backToKeyConfigs browserKeyMapAnyMatchers
        }

data ReportsKeyConfigs = ReportsKeyConfigs
  { nextActionReportKeyConfigs :: Maybe NextActionReportKeyConfigs,
    waitingReportKeyConfigs :: Maybe WaitingReportKeyConfigs,
    timestampsReportKeyConfigs :: Maybe TimestampsReportKeyConfigs,
    stuckReportKeyConfigs :: Maybe StuckReportKeyConfigs,
    workReportKeyConfigs :: Maybe WorkReportKeyConfigs,
    anyReportKeyConfigs :: Maybe KeyConfigs
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec ReportsKeyConfigs)

instance Validity ReportsKeyConfigs

instance HasCodec ReportsKeyConfigs where
  codec =
    object "ReportsKeyConfigs" $
      ReportsKeyConfigs
        <$> optionalFieldOrNull "next-action" "Keybindings for the interactive next action report" .= nextActionReportKeyConfigs
        <*> optionalFieldOrNull "waiting" "Keybindings for the interactive waiting report" .= waitingReportKeyConfigs
        <*> optionalFieldOrNull "timestamps" "Keybindings for the interactive timestamps report" .= timestampsReportKeyConfigs
        <*> optionalFieldOrNull "stuck" "Keybindings for the interactive stuck projects report" .= stuckReportKeyConfigs
        <*> optionalFieldOrNull "work" "Keybindings for the interactive work report" .= workReportKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in any report" .= anyReportKeyConfigs

backToReportsKeyConfig :: ReportsKeyMap -> ReportsKeyConfigs
backToReportsKeyConfig ReportsKeyMap {..} =
  let ReportsKeyMap _ _ _ _ _ _ = undefined
   in ReportsKeyConfigs
        { nextActionReportKeyConfigs = Just $ backToNextActionReportKeyConfigs reportsKeymapNextActionReportKeyMap,
          waitingReportKeyConfigs = Just $ backToWaitingReportKeyConfigs reportsKeymapWaitingReportKeyMap,
          timestampsReportKeyConfigs = Just $ backToTimestampsReportKeyConfigs reportsKeymapTimestampsReportKeyMap,
          stuckReportKeyConfigs = Just $ backToStuckReportKeyConfigs reportsKeymapStuckReportKeyMap,
          workReportKeyConfigs = Just $ backToWorkReportKeyConfigs reportsKeymapWorkReportKeyMap,
          anyReportKeyConfigs = Just $ backToKeyConfigs reportsKeymapAnyMatchers
        }

data NextActionReportKeyConfigs = NextActionReportKeyConfigs
  { nextActionReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    nextActionReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    nextActionReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec NextActionReportKeyConfigs)

instance Validity NextActionReportKeyConfigs

instance HasCodec NextActionReportKeyConfigs where
  codec =
    object "NextActionReportKeyConfigs" $
      NextActionReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the next-action report" .= nextActionReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the next-action report" .= nextActionReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the next-action report" .= nextActionReportAnyKeyConfigs

backToNextActionReportKeyConfigs :: NextActionReportKeyMap -> NextActionReportKeyConfigs
backToNextActionReportKeyConfigs NextActionReportKeyMap {..} =
  let NextActionReportKeyMap _ _ _ = undefined
   in NextActionReportKeyConfigs
        { nextActionReportNormalKeyConfigs = Just $ backToKeyConfigs nextActionReportMatchers,
          nextActionReportSearchKeyConfigs = Just $ backToKeyConfigs nextActionReportSearchMatchers,
          nextActionReportAnyKeyConfigs = Just $ backToKeyConfigs nextActionReportAnyMatchers
        }

data WaitingReportKeyConfigs = WaitingReportKeyConfigs
  { waitingReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    waitingReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    waitingReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec WaitingReportKeyConfigs)

instance Validity WaitingReportKeyConfigs

instance HasCodec WaitingReportKeyConfigs where
  codec =
    object "WaitingReportKeyConfigs" $
      WaitingReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the waiting report" .= waitingReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the waiting report" .= waitingReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the waiting report" .= waitingReportAnyKeyConfigs

backToWaitingReportKeyConfigs :: WaitingReportKeyMap -> WaitingReportKeyConfigs
backToWaitingReportKeyConfigs WaitingReportKeyMap {..} =
  let WaitingReportKeyMap _ _ _ = undefined
   in WaitingReportKeyConfigs
        { waitingReportNormalKeyConfigs = Just $ backToKeyConfigs waitingReportMatchers,
          waitingReportSearchKeyConfigs = Just $ backToKeyConfigs waitingReportSearchMatchers,
          waitingReportAnyKeyConfigs = Just $ backToKeyConfigs waitingReportAnyMatchers
        }

data TimestampsReportKeyConfigs = TimestampsReportKeyConfigs
  { timestampsReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    timestampsReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    timestampsReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec TimestampsReportKeyConfigs)

instance Validity TimestampsReportKeyConfigs

instance HasCodec TimestampsReportKeyConfigs where
  codec =
    object "TimestampsReportKeyConfigs" $
      TimestampsReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the timestamps report" .= timestampsReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the timestamps report" .= timestampsReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the timestamps report" .= timestampsReportAnyKeyConfigs

backToTimestampsReportKeyConfigs :: TimestampsReportKeyMap -> TimestampsReportKeyConfigs
backToTimestampsReportKeyConfigs TimestampsReportKeyMap {..} =
  let TimestampsReportKeyMap _ _ _ = undefined
   in TimestampsReportKeyConfigs
        { timestampsReportNormalKeyConfigs = Just $ backToKeyConfigs timestampsReportMatchers,
          timestampsReportSearchKeyConfigs = Just $ backToKeyConfigs timestampsReportSearchMatchers,
          timestampsReportAnyKeyConfigs = Just $ backToKeyConfigs timestampsReportAnyMatchers
        }

data StuckReportKeyConfigs = StuckReportKeyConfigs
  { stuckReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    stuckReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec StuckReportKeyConfigs)

instance Validity StuckReportKeyConfigs

instance HasCodec StuckReportKeyConfigs where
  codec =
    object "StuckReportKeyConfigs" $
      StuckReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the stuck report" .= stuckReportNormalKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the stuck report" .= stuckReportAnyKeyConfigs

backToStuckReportKeyConfigs :: StuckReportKeyMap -> StuckReportKeyConfigs
backToStuckReportKeyConfigs StuckReportKeyMap {..} =
  let StuckReportKeyMap _ _ = undefined
   in StuckReportKeyConfigs
        { stuckReportNormalKeyConfigs = Just $ backToKeyConfigs stuckReportMatchers,
          stuckReportAnyKeyConfigs = Just $ backToKeyConfigs stuckReportAnyMatchers
        }

data WorkReportKeyConfigs = WorkReportKeyConfigs
  { workReportNormalKeyConfigs :: !(Maybe KeyConfigs),
    workReportSearchKeyConfigs :: !(Maybe KeyConfigs),
    workReportAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec WorkReportKeyConfigs)

instance Validity WorkReportKeyConfigs

instance HasCodec WorkReportKeyConfigs where
  codec =
    object "WorkReportKeyConfigs" $
      WorkReportKeyConfigs
        <$> optionalFieldOrNull "normal" "Keybindings for interacting with the work report" .= workReportNormalKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for the search in the work report" .= workReportSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any point in the work report" .= workReportAnyKeyConfigs

backToWorkReportKeyConfigs :: WorkReportKeyMap -> WorkReportKeyConfigs
backToWorkReportKeyConfigs WorkReportKeyMap {..} =
  let WorkReportKeyMap _ _ _ = undefined
   in WorkReportKeyConfigs
        { workReportNormalKeyConfigs = Just $ backToKeyConfigs workReportMatchers,
          workReportSearchKeyConfigs = Just $ backToKeyConfigs workReportSearchMatchers,
          workReportAnyKeyConfigs = Just $ backToKeyConfigs workReportAnyMatchers
        }

data HelpKeyConfigs = HelpKeyConfigs
  { helpHelpKeyConfigs :: !(Maybe KeyConfigs),
    helpSearchKeyConfigs :: !(Maybe KeyConfigs),
    helpAnyKeyConfigs :: !(Maybe KeyConfigs)
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec HelpKeyConfigs)

instance Validity HelpKeyConfigs

instance HasCodec HelpKeyConfigs where
  codec =
    object "HelpKeyConfigs" $
      HelpKeyConfigs
        <$> optionalFieldOrNull "help" "Keybindings for when in the help screen" .= helpHelpKeyConfigs
        <*> optionalFieldOrNull "search" "Keybindings for when the search bar is selected within the help screen" .= helpSearchKeyConfigs
        <*> optionalFieldOrNull "any" "Keybindings for at any time in the help screen" .= helpAnyKeyConfigs

backToHelpKeyConfigs :: HelpKeyMap -> HelpKeyConfigs
backToHelpKeyConfigs HelpKeyMap {..} =
  let HelpKeyMap _ _ _ = undefined
   in HelpKeyConfigs
        { helpHelpKeyConfigs = Just $ backToKeyConfigs helpKeyMapHelpMatchers,
          helpSearchKeyConfigs = Just $ backToKeyConfigs helpKeyMapSearchMatchers,
          helpAnyKeyConfigs = Just $ backToKeyConfigs helpKeyMapAnyMatchers
        }

newtype KeyConfigs = KeyConfigs
  { keyConfigs :: [KeyConfig]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Validity)
  deriving (ToJSON, FromJSON) via (Autodocodec KeyConfigs)

instance HasCodec KeyConfigs where
  codec = named "KeyConfigs" $ dimapCodec KeyConfigs keyConfigs codec

backToKeyConfigs :: KeyMappings -> KeyConfigs
backToKeyConfigs kms = KeyConfigs {keyConfigs = map backToKeyConfig kms}

data KeyConfig = KeyConfig
  { keyConfigMatcher :: !MatcherConfig,
    keyConfigAction :: !ActionName
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec KeyConfig)

instance Validity KeyConfig

instance HasCodec KeyConfig where
  codec =
    named "KeyConfig" $
      object "KeyConfig" $
        KeyConfig
          <$> requiredField "key" "The key to match" .= keyConfigMatcher
          <*> requiredField "action" "The name of the action to perform when the key is matched" .= keyConfigAction

backToKeyConfig :: KeyMapping -> KeyConfig
backToKeyConfig km =
  case km of
    MapVtyExactly kp a ->
      KeyConfig {keyConfigMatcher = MatchConfKeyPress kp, keyConfigAction = actionName a}
    MapAnyTypeableChar au ->
      KeyConfig {keyConfigMatcher = MatchConfAnyChar, keyConfigAction = actionUsingName au}
    MapCatchAll a ->
      KeyConfig {keyConfigMatcher = MatchConfCatchAll, keyConfigAction = actionName a}
    MapCombination kp_ km_ ->
      let go km__ =
            case km__ of
              MapVtyExactly kp__ a_ -> (MatchConfKeyPress kp__, actionName a_)
              MapAnyTypeableChar au -> (MatchConfAnyChar, actionUsingName au)
              MapCatchAll a_ -> (MatchConfCatchAll, actionName a_)
              MapCombination kp__ km___ ->
                let (mc_, a_) = go km___
                 in (MatchConfCombination kp__ mc_, a_)
          (mc, a) = go km_
       in KeyConfig {keyConfigMatcher = MatchConfCombination kp_ mc, keyConfigAction = a}

data Instructions
  = Instructions (Maybe StartingPath) SmosConfig

data CombineError
  = ActionNotFound ActionName
  | ActionWrongType ActionName
  deriving (Show, Eq, Generic)

data Comb a
  = Combined a
  | CombErr [CombineError]
  deriving (Show, Eq, Generic)

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
