{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projects where

import GHC.Generics (Generic)

import Data.Validity

import Smos.Data

import Smos.Report.Path

data ProjectEntry = ProjectEntry
    { projectEntryFilePath :: RootedPath
    , projectEntryCurrentEntry :: Maybe Entry
    } deriving (Show, Eq, Generic)

instance Validity ProjectEntry

makeProjectEntry :: RootedPath -> SmosFile -> ProjectEntry
makeProjectEntry rp sf =
    ProjectEntry
    {projectEntryFilePath = rp, projectEntryCurrentEntry = getCurrentEntry sf}

getCurrentEntry :: SmosFile -> Maybe Entry
getCurrentEntry = goF . smosFileForest
  where
    goF f =
        case reverse f of
            [] -> Nothing
            (lt:_) -> goT lt
    goT (Node e f) =
        case reverse f of
            [] -> Just e
            _ -> goF f
