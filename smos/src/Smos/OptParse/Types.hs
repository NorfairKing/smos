{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import qualified Data.Text as T
import Data.Yaml as Yaml

import Graphics.Vty.Input.Events

import qualified Smos.Report.OptParse.Types as Report

import Smos.Types

data Arguments =
    Arguments FilePath
              Flags

data Flags = Flags
    { flagConfigFile :: !(Maybe FilePath)
    , flagReportFlags :: !Report.Flags
    } deriving (Show, Eq)

data Environment = Environment
    { envConfigFile :: !(Maybe FilePath)
    , envReportEnv :: !Report.Environment
    } deriving (Show, Eq)

data Configuration = Configuration
    { confReportConf :: !Report.Configuration
    , confKeybindingsConf :: !KeybindingsConfiguration
    } deriving (Show, Eq, Generic)

instance FromJSON Configuration where
    parseJSON v =
        flip (withObject "Configuration") v $ \o ->
            Configuration <$> parseJSON v <*> o .: "keys"

data KeybindingsConfiguration = KeybindingsConfiguration
    { confReset :: !(Maybe Bool)
    , confFileKeyConfig :: !(Maybe FileKeyConfigs)
    , confReportsKeyConfig :: !(Maybe ReportsKeyConfigs)
    , confHelpKeyConfig :: !(Maybe KeyConfigs)
    } deriving (Show, Eq, Generic)

instance FromJSON KeybindingsConfiguration where
    parseJSON =
        withObject "KeybindingsConfiguration" $ \o ->
            KeybindingsConfiguration <$> o .:? "reset" <*> o .:? "file" <*>
            o .:? "reports" <*>
            o .:? "help"

data FileKeyConfigs = FileKeyConfigs
    { emptyKeyConfigs :: !(Maybe KeyConfigs)
    , entryKeyConfigs :: !(Maybe KeyConfigs)
    , headerKeyConfigs :: !(Maybe KeyConfigs)
    , contentsKeyConfigs :: !(Maybe KeyConfigs)
    , timestampsKeyConfigs :: !(Maybe KeyConfigs)
    , propertiesKeyConfigs :: !(Maybe KeyConfigs)
    , stateHistoryKeyConfigs :: !(Maybe KeyConfigs)
    , tagsKeyConfigs :: !(Maybe KeyConfigs)
    , logbookKeyConfigs :: !(Maybe KeyConfigs)
    , anyKeyConfigs :: !(Maybe KeyConfigs)
    } deriving (Show, Eq, Generic)

instance FromJSON FileKeyConfigs where
    parseJSON =
        withObject "FileKeyConfigs" $ \o ->
            FileKeyConfigs <$> o .:? "empty" <*> o .:? "entry" <*>
            o .:? "header" <*>
            o .:? "contents" <*>
            o .:? "timestamps" <*>
            o .:? "properties" <*>
            o .:? "state-history" <*>
            o .:? "tags" <*>
            o .:? "logbook" <*>
            o .:? "any"

data ReportsKeyConfigs = ReportsKeyConfigs
    { nextActionReportKeyConfigs :: !(Maybe KeyConfigs)
    } deriving (Show, Eq, Generic)

instance FromJSON ReportsKeyConfigs where
    parseJSON =
        withObject "ReportsKeyConfigs" $ \o ->
            ReportsKeyConfigs <$> o .:? "next-action"

newtype KeyConfigs = KeyConfigs
    { keyConfigs :: [KeyConfig]
    } deriving (Show, Eq, Generic, FromJSON)

data KeyConfig = KeyConfig
    { keyConfigMatcher :: !MatcherConfig
    , keyConfigAction :: !ActionName
    } deriving (Show, Eq, Generic)

instance FromJSON KeyConfig where
    parseJSON =
        withObject "KeyConfig" $ \o ->
            KeyConfig <$> o .: "key" <*> o .: "action"

data MatcherConfig
    = MatchConfKeyPress !KeyPress
    | MatchConfAnyChar
    | MatchConfCatchAll
    deriving (Show, Eq, Generic)

instance FromJSON MatcherConfig where
    parseJSON =
        withText "MatcherConfig" $ \t ->
            case T.unpack t of
                [c] -> pure $ MatchConfKeyPress (KeyPress (KChar c) [])
                "char" -> pure MatchConfAnyChar
                "any" -> pure MatchConfCatchAll
                _ -> fail "Unknown key matcher"

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig

data CombineError =
    ActionNotFound ActionName | ActionWrongType ActionName
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
