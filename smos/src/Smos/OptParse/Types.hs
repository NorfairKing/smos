{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.OptParse.Types where

import Autodocodec
import Data.Validity
import GHC.Generics (Generic)
import Smos.CLI.OptParse
import Smos.Keys
import qualified Smos.Report.OptParse.Types as Report
import Smos.Types

data Arguments
  = Arguments (Maybe FilePath) (FlagsWithConfigFile Flags)

data Flags = Flags
  { flagReportFlags :: !Report.Flags,
    flagExplainerMode :: !(Maybe Bool),
    flagSandboxMode :: !(Maybe Bool)
  }

data Environment = Environment
  { envReportEnvironment :: !Report.Environment,
    envExplainerMode :: !(Maybe Bool),
    envSandboxMode :: !(Maybe Bool)
  }

data Configuration = Configuration
  { confReportConf :: !Report.Configuration,
    confKeybindingsConf :: !(Maybe KeybindingsConfiguration),
    confExplainerMode :: !(Maybe Bool),
    confSandboxMode :: !(Maybe Bool)
  }
  deriving stock (Show, Generic)

instance Validity Configuration

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec
          .= confReportConf
        <*> optionalFieldOrNull "keys" "Keybindings"
          .= confKeybindingsConf
        <*> optionalFieldOrNull "explainer-mode" "Turn on explainer mode where the user can see what is happening"
          .= confExplainerMode
        <*> optionalFieldOrNull "sandbox-mode" "Turn on sandbox mode where smos cannot affect any files other than the workflow files"
          .= confSandboxMode

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

instance HasCodec KeybindingsConfiguration where
  codec =
    object "KeybindingsConfiguration" $
      KeybindingsConfiguration
        <$> optionalFieldOrNull "reset" "Whether to reset all keybindings. Set this to false to add keys, set this to true to replace keys."
          .= confReset
        <*> optionalFieldOrNull "file" "Keybindings for the file context"
          .= confFileKeyConfig
        <*> optionalFieldOrNull "browser" "Keybindings for the file browser context"
          .= confBrowserKeyConfig
        <*> optionalFieldOrNull "reports" "Keybindings for the reports context"
          .= confReportsKeyConfig
        <*> optionalFieldOrNull "help" "Keybindings for the help context"
          .= confHelpKeyConfig
        <*> optionalFieldOrNull "any" "Keybindings for any context"
          .= confAnyKeyConfig

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

data Instructions
  = Instructions (Maybe StartingPath) SmosConfig

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
