{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse.Types where

import Data.Aeson as JSON
import Import
import Smos.Keys
import qualified Smos.Report.OptParse.Types as Report
import Smos.Types
import YamlParse.Applicative

data Arguments
  = Arguments FilePath (Report.FlagsWithConfigFile Flags)

data Flags
  = Flags
      { flagReportFlags :: Report.Flags,
        flagExplainerMode :: Maybe Bool
      }
  deriving (Show, Eq)

data Environment
  = Environment
      { envReportEnvironment :: Report.Environment,
        envExplainerMode :: Maybe Bool
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confReportConf :: !Report.Configuration,
        confKeybindingsConf :: !(Maybe KeybindingsConfiguration),
        confExplainerMode :: !(Maybe Bool)
      }
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    toJSON confReportConf
      `mergeObjects` object
        [ "keys" .= confKeybindingsConf,
          "explainer-mode" .= confExplainerMode
        ]

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    Configuration
      <$> yamlSchema
      <*> objectParser "Configuration" (optionalField "keys" "Keybindings")
      <*> objectParser "Configuration" (optionalField "explainer-mode" "Turn on explainer mode where the user can see what is happening")

backToConfiguration :: SmosConfig -> Configuration
backToConfiguration SmosConfig {..} =
  let SmosConfig _ _ _ = undefined
   in Configuration
        { confReportConf = Report.backToConfiguration configReportConfig,
          confKeybindingsConf = Just $ backToKeybindingsConfiguration configKeyMap,
          confExplainerMode = if configExplainerMode then Just True else Nothing
        }

data KeybindingsConfiguration
  = KeybindingsConfiguration
      { confReset :: !(Maybe Bool),
        confFileKeyConfig :: !(Maybe FileKeyConfigs),
        confBrowserKeyConfig :: !(Maybe BrowserKeyConfigs),
        confReportsKeyConfig :: !(Maybe ReportsKeyConfigs),
        confHelpKeyConfig :: !(Maybe HelpKeyConfigs),
        confAnyKeyConfig :: !(Maybe KeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity KeybindingsConfiguration

instance ToJSON KeybindingsConfiguration where
  toJSON KeybindingsConfiguration {..} =
    object
      [ "reset" .= confReset,
        "file" .= confFileKeyConfig,
        "browser" .= confBrowserKeyConfig,
        "reports" .= confReportsKeyConfig,
        "help" .= confHelpKeyConfig,
        "any" .= confAnyKeyConfig
      ]

instance FromJSON KeybindingsConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema KeybindingsConfiguration where
  yamlSchema =
    objectParser "KeybindingsConfiguration" $
      KeybindingsConfiguration
        <$> optionalField "reset" "Whether to reset all keybindings. Set this to false to add keys, set this to true to replace keys."
        <*> optionalField "file" "Keybindings for the file context"
        <*> optionalField "browser" "Keybindings for the file browser context"
        <*> optionalField "reports" "Keybindings for the reports context"
        <*> optionalField "help" "Keybindings for the help context"
        <*> optionalField "any" "Keybindings for any context"

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

data FileKeyConfigs
  = FileKeyConfigs
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
  deriving (Show, Eq, Generic)

instance Validity FileKeyConfigs

instance ToJSON FileKeyConfigs where
  toJSON FileKeyConfigs {..} =
    object
      [ "empty" .= emptyKeyConfigs,
        "entry" .= entryKeyConfigs,
        "header" .= headerKeyConfigs,
        "contents" .= contentsKeyConfigs,
        "timestamps" .= timestampsKeyConfigs,
        "properties" .= propertiesKeyConfigs,
        "state-history" .= stateHistoryKeyConfigs,
        "tags" .= tagsKeyConfigs,
        "logbook" .= logbookKeyConfigs,
        "any" .= anyKeyConfigs
      ]

instance FromJSON FileKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema FileKeyConfigs where
  yamlSchema =
    objectParser "FileKeyConfigs" $
      FileKeyConfigs
        <$> optionalField "empty" "Keybindings for when the file is empty"
        <*> optionalField "entry" "Keybindings for when an entry is selected"
        <*> optionalField "header" "Keybindings for when an header is selected"
        <*> optionalField "contents" "Keybindings for when an contents is selected"
        <*> optionalField "timestamps" "Keybindings for when a timestamps are selected"
        <*> optionalField "properties" "Keybindings for when a properties are selected"
        <*> optionalField "state-history" "Keybindings for when a state history is selected"
        <*> optionalField "tags" "Keybindings for when a tags are selected"
        <*> optionalField "logbook" "Keybindings for when a logbook is selected"
        <*> optionalField "any" "Keybindings that match in any file subcontext"

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

data BrowserKeyConfigs
  = BrowserKeyConfigs
      { browserExistentKeyConfigs :: Maybe KeyConfigs,
        browserInProgressKeyConfigs :: Maybe KeyConfigs,
        browserEmptyKeyConfigs :: Maybe KeyConfigs,
        browserAnyKeyConfigs :: Maybe KeyConfigs
      }
  deriving (Show, Eq, Generic)

instance Validity BrowserKeyConfigs

instance ToJSON BrowserKeyConfigs where
  toJSON BrowserKeyConfigs {..} =
    let BrowserKeyMap _ _ _ _ = undefined
     in object
          [ "existent" .= browserExistentKeyConfigs,
            "in-progress" .= browserInProgressKeyConfigs,
            "empty" .= browserEmptyKeyConfigs,
            "any" .= browserAnyKeyConfigs
          ]

instance FromJSON BrowserKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema BrowserKeyConfigs where
  yamlSchema =
    objectParser "BrowserKeyConfigs" $
      BrowserKeyConfigs
        <$> optionalField "existent" "Keybindings for when an existing file or directory is selected"
        <*> optionalField "in-progress" "Keybindings for when an in-progress file or directory is selected"
        <*> optionalField "empty" "Keybindings for when the directory being browsed is empty"
        <*> optionalField "any" "Keybindings for any of the other file browser situations"

backToBrowserKeyConfigs :: BrowserKeyMap -> BrowserKeyConfigs
backToBrowserKeyConfigs BrowserKeyMap {..} =
  let BrowserKeyMap _ _ _ _ = undefined
   in BrowserKeyConfigs
        { browserExistentKeyConfigs = Just $ backToKeyConfigs browserKeyMapExistentMatchers,
          browserInProgressKeyConfigs = Just $ backToKeyConfigs browserKeyMapInProgressMatchers,
          browserEmptyKeyConfigs = Just $ backToKeyConfigs browserKeyMapEmptyMatchers,
          browserAnyKeyConfigs = Just $ backToKeyConfigs browserKeyMapAnyMatchers
        }

data ReportsKeyConfigs
  = ReportsKeyConfigs
      { nextActionReportKeyConfigs :: Maybe NextActionReportKeyConfigs,
        waitingReportKeyConfigs :: Maybe WaitingReportKeyConfigs,
        anyReportKeyConfigs :: Maybe KeyConfigs
      }
  deriving (Show, Eq, Generic)

instance Validity ReportsKeyConfigs

instance ToJSON ReportsKeyConfigs where
  toJSON ReportsKeyConfigs {..} =
    let ReportsKeyConfigs _ _ _ = undefined
     in object
          [ "next-action" .= nextActionReportKeyConfigs,
            "waiting" .= waitingReportKeyConfigs,
            "any" .= anyReportKeyConfigs
          ]

instance FromJSON ReportsKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema ReportsKeyConfigs where
  yamlSchema =
    objectParser "ReportsKeyConfigs" $
      ReportsKeyConfigs
        <$> optionalField "next-action" "Keybindings for the interactive next action report"
        <*> optionalField "waiting" "Keybindings for the interactive waiting report"
        <*> optionalField "any" "Keybindings for at any point in any report"

backToReportsKeyConfig :: ReportsKeyMap -> ReportsKeyConfigs
backToReportsKeyConfig ReportsKeyMap {..} =
  let ReportsKeyMap _ _ _ = undefined
   in ReportsKeyConfigs
        { nextActionReportKeyConfigs = Just $ backToNextActionReportKeyConfigs reportsKeymapNextActionReportKeyMap,
          waitingReportKeyConfigs = Just $ backToWaitingReportKeyConfigs reportsKeymapWaitingReportKeyMap,
          anyReportKeyConfigs = Just $ backToKeyConfigs reportsKeymapAnyMatchers
        }

data NextActionReportKeyConfigs
  = NextActionReportKeyConfigs
      { nextActionReportNormalKeyConfigs :: !(Maybe KeyConfigs),
        nextActionReportSearchKeyConfigs :: !(Maybe KeyConfigs),
        nextActionReportAnyKeyConfigs :: !(Maybe KeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity NextActionReportKeyConfigs

instance ToJSON NextActionReportKeyConfigs where
  toJSON NextActionReportKeyConfigs {..} =
    let NextActionReportKeyConfigs _ _ _ = undefined
     in object
          [ "normal" .= nextActionReportNormalKeyConfigs,
            "search" .= nextActionReportSearchKeyConfigs,
            "any" .= nextActionReportAnyKeyConfigs
          ]

instance FromJSON NextActionReportKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema NextActionReportKeyConfigs where
  yamlSchema =
    objectParser "NextActionReportKeyConfigs" $
      NextActionReportKeyConfigs
        <$> optionalField "normal" "Keybindings for interacting with the next-action report"
        <*> optionalField "search" "Keybindings for the search in the next-action report"
        <*> optionalField "any" "Keybindings for at any point in the next action report"

backToNextActionReportKeyConfigs :: NextActionReportKeyMap -> NextActionReportKeyConfigs
backToNextActionReportKeyConfigs NextActionReportKeyMap {..} =
  let NextActionReportKeyMap _ _ _ = undefined
   in NextActionReportKeyConfigs
        { nextActionReportNormalKeyConfigs = Just $ backToKeyConfigs nextActionReportMatchers,
          nextActionReportSearchKeyConfigs = Just $ backToKeyConfigs nextActionReportSearchMatchers,
          nextActionReportAnyKeyConfigs = Just $ backToKeyConfigs nextActionReportAnyMatchers
        }

data WaitingReportKeyConfigs
  = WaitingReportKeyConfigs
      { waitingReportNormalKeyConfigs :: !(Maybe KeyConfigs),
        waitingReportAnyKeyConfigs :: !(Maybe KeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity WaitingReportKeyConfigs

instance ToJSON WaitingReportKeyConfigs where
  toJSON WaitingReportKeyConfigs {..} =
    let WaitingReportKeyConfigs _ _ = undefined
     in object
          [ "normal" .= waitingReportNormalKeyConfigs,
            "any" .= waitingReportAnyKeyConfigs
          ]

instance FromJSON WaitingReportKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema WaitingReportKeyConfigs where
  yamlSchema =
    objectParser "WaitingReportKeyConfigs" $
      WaitingReportKeyConfigs
        <$> optionalField "normal" "Keybindings for interacting with the next-action report"
        <*> optionalField "any" "Keybindings for at any point in the next action report"

backToWaitingReportKeyConfigs :: WaitingReportKeyMap -> WaitingReportKeyConfigs
backToWaitingReportKeyConfigs WaitingReportKeyMap {..} =
  let WaitingReportKeyMap _ _ = undefined
   in WaitingReportKeyConfigs
        { waitingReportNormalKeyConfigs = Just $ backToKeyConfigs waitingReportMatchers,
          waitingReportAnyKeyConfigs = Just $ backToKeyConfigs waitingReportAnyMatchers
        }

data HelpKeyConfigs
  = HelpKeyConfigs
      { helpHelpKeyConfigs :: !(Maybe KeyConfigs),
        helpSearchKeyConfigs :: !(Maybe KeyConfigs),
        helpAnyKeyConfigs :: !(Maybe KeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity HelpKeyConfigs

instance ToJSON HelpKeyConfigs where
  toJSON HelpKeyConfigs {..} =
    let HelpKeyConfigs _ _ _ = undefined
     in object
          [ "help" .= helpHelpKeyConfigs,
            "search" .= helpSearchKeyConfigs,
            "any" .= helpAnyKeyConfigs
          ]

instance FromJSON HelpKeyConfigs where
  parseJSON = viaYamlSchema

instance YamlSchema HelpKeyConfigs where
  yamlSchema =
    objectParser "HelpKeyConfigs" $
      HelpKeyConfigs
        <$> optionalField "help" "Keybindings for when in the help screen"
        <*> optionalField "search" "Keybindings for when the search bar is selected within the help screen"
        <*> optionalField "any" "Keybindings for at any time in the help screen"

backToHelpKeyConfigs :: HelpKeyMap -> HelpKeyConfigs
backToHelpKeyConfigs HelpKeyMap {..} =
  let HelpKeyMap _ _ _ = undefined
   in HelpKeyConfigs
        { helpHelpKeyConfigs = Just $ backToKeyConfigs helpKeyMapHelpMatchers,
          helpSearchKeyConfigs = Just $ backToKeyConfigs helpKeyMapSearchMatchers,
          helpAnyKeyConfigs = Just $ backToKeyConfigs helpKeyMapAnyMatchers
        }

newtype KeyConfigs
  = KeyConfigs
      { keyConfigs :: [KeyConfig]
      }
  deriving (Show, Eq, Generic, Validity, ToJSON, FromJSON)

instance YamlSchema KeyConfigs where
  yamlSchema = KeyConfigs <$> yamlSchema

backToKeyConfigs :: KeyMappings -> KeyConfigs
backToKeyConfigs kms = KeyConfigs {keyConfigs = map backToKeyConfig kms}

data KeyConfig
  = KeyConfig
      { keyConfigMatcher :: !MatcherConfig,
        keyConfigAction :: !ActionName
      }
  deriving (Show, Eq, Generic)

instance Validity KeyConfig

instance ToJSON KeyConfig where
  toJSON KeyConfig {..} = object ["key" .= keyConfigMatcher, "action" .= keyConfigAction]

instance FromJSON KeyConfig where
  parseJSON = viaYamlSchema

instance YamlSchema KeyConfig where
  yamlSchema = objectParser "KeyConfig" $ KeyConfig <$> requiredField "key" "The key to match" <*> requiredField "action" "The name of the action to perform when the key is matched"

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
  = Instructions (Path Abs File) SmosConfig

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

mergeObjects :: Value -> Value -> Value
mergeObjects (Object hm1) (Object hm2) = Object $ hm1 <> hm2
mergeObjects v1 _ = v1
