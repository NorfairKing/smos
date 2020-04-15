{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.OptParse.Types where

import Data.Aeson as JSON
import Import
import Smos.Keys
import qualified Smos.Report.OptParse.Types as Report
import Smos.Types

data Arguments
  = Arguments FilePath Flags

newtype Flags
  = Flags
      { flagReportFlags :: Report.Flags
      }
  deriving (Show, Eq)

newtype Environment
  = Environment
      { envReportEnvironment :: Report.Environment
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confReportConf :: !Report.Configuration,
        confKeybindingsConf :: !(Maybe KeybindingsConfiguration)
      }
  deriving (Show, Eq, Generic)

instance Validity Configuration

instance ToJSON Configuration where
  toJSON Configuration {..} =
    toJSON confReportConf `mergeObjects` object ["keys" .= confKeybindingsConf]

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o -> Configuration <$> parseJSON v <*> o .:? "keys"

backToConfiguration :: SmosConfig -> Configuration
backToConfiguration SmosConfig {..} =
  Configuration
    { confReportConf = Report.backToConfiguration configReportConfig,
      confKeybindingsConf = Just $ backToKeybindingsConfiguration configKeyMap
    }

data KeybindingsConfiguration
  = KeybindingsConfiguration
      { confReset :: !(Maybe Bool),
        confFileKeyConfig :: !(Maybe FileKeyConfigs),
        confReportsKeyConfig :: !(Maybe ReportsKeyConfigs),
        confHelpKeyConfig :: !(Maybe HelpKeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity KeybindingsConfiguration

instance ToJSON KeybindingsConfiguration where
  toJSON KeybindingsConfiguration {..} =
    object
      [ "reset" .= confReset,
        "file" .= confFileKeyConfig,
        "reports" .= confReportsKeyConfig,
        "help" .= confHelpKeyConfig
      ]

instance FromJSON KeybindingsConfiguration where
  parseJSON =
    withObject "KeybindingsConfiguration" $ \o ->
      KeybindingsConfiguration <$> o .:? "reset" <*> o .:? "file" <*> o .:? "reports"
        <*> o
        .:? "help"

backToKeybindingsConfiguration :: KeyMap -> KeybindingsConfiguration
backToKeybindingsConfiguration KeyMap {..} =
  KeybindingsConfiguration
    { confReset = Just True,
      confFileKeyConfig = Just $ backToFileKeyConfigs keyMapFileKeyMap,
      confReportsKeyConfig = Just $ backToReportsKeyConfig keyMapReportsKeyMap,
      confHelpKeyConfig = Just $ backToHelpKeyConfigs keyMapHelpKeyMap
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
  parseJSON =
    withObject "FileKeyConfigs" $ \o ->
      FileKeyConfigs <$> o .:? "empty" <*> o .:? "entry" <*> o .:? "header" <*> o .:? "contents"
        <*> o .:? "timestamps"
        <*> o .:? "properties"
        <*> o .:? "state-history"
        <*> o .:? "tags"
        <*> o .:? "logbook"
        <*> o .:? "any"

backToFileKeyConfigs :: FileKeyMap -> FileKeyConfigs
backToFileKeyConfigs FileKeyMap {..} =
  FileKeyConfigs
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

newtype ReportsKeyConfigs
  = ReportsKeyConfigs
      { nextActionReportKeyConfigs :: Maybe KeyConfigs
      }
  deriving (Show, Eq, Generic)

instance Validity ReportsKeyConfigs

instance ToJSON ReportsKeyConfigs where
  toJSON ReportsKeyConfigs {..} = object ["next-action" .= nextActionReportKeyConfigs]

instance FromJSON ReportsKeyConfigs where
  parseJSON = withObject "ReportsKeyConfigs" $ \o -> ReportsKeyConfigs <$> o .:? "next-action"

backToReportsKeyConfig :: ReportsKeyMap -> ReportsKeyConfigs
backToReportsKeyConfig ReportsKeyMap {..} =
  ReportsKeyConfigs
    { nextActionReportKeyConfigs = Just $ backToKeyConfigs reportsKeymapNextActionReportMatchers
    }

data HelpKeyConfigs
  = HelpKeyConfigs
      { helpHelpKeyConfigs :: !(Maybe KeyConfigs),
        helpSearchKeyConfigs :: !(Maybe KeyConfigs)
      }
  deriving (Show, Eq, Generic)

instance Validity HelpKeyConfigs

instance ToJSON HelpKeyConfigs where
  toJSON HelpKeyConfigs {..} =
    object ["help" .= helpHelpKeyConfigs, "search" .= helpSearchKeyConfigs]

instance FromJSON HelpKeyConfigs where
  parseJSON = withObject "HelpKeyConfigs" $ \o -> HelpKeyConfigs <$> o .:? "help" <*> o .:? "search"

backToHelpKeyConfigs :: HelpKeyMap -> HelpKeyConfigs
backToHelpKeyConfigs HelpKeyMap {..} =
  HelpKeyConfigs
    { helpHelpKeyConfigs = Just $ backToKeyConfigs helpKeyMapHelpMatchers,
      helpSearchKeyConfigs = Just $ backToKeyConfigs helpKeyMapSearchMatchers
    }

newtype KeyConfigs
  = KeyConfigs
      { keyConfigs :: [KeyConfig]
      }
  deriving (Show, Eq, Generic, Validity, ToJSON, FromJSON)

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
  parseJSON = withObject "KeyConfig" $ \o -> KeyConfig <$> o .: "key" <*> o .: "action"

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
