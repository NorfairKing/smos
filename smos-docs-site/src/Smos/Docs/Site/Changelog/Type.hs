{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.Changelog.Type where

import Data.Data
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Data.Version
import Data.Yaml
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax

newtype Changelog
  = Changelog
      { changelogVersions :: [ChangelogVersion]
      }
  deriving (Show, Eq, Generic, Data)

instance Lift Changelog

instance FromJSON Changelog where
  parseJSON v = Changelog <$> parseJSON v

data ChangelogVersion
  = ChangelogVersion
      { changelogVersionDate :: Day,
        changelogVersionVersions :: Map String Version,
        changelogVersionAdditions :: Map String String,
        changelogVersionChanges :: Map String String,
        changelogVersionFixes :: Map String String,
        changelogVersionDeprecations :: Map String String
      }
  deriving (Show, Eq, Generic, Data)

instance Lift ChangelogVersion

instance FromJSON ChangelogVersion where
  parseJSON = withObject "ChangelogVersion" $ \o ->
    ChangelogVersion
      <$> o .: "date"
      <*> o .: "versions"
      <*> o .:? "added" .!= M.empty
      <*> o .:? "changed" .!= M.empty
      <*> o .:? "fixed" .!= M.empty
      <*> o .:? "deprecated" .!= M.empty
