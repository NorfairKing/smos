{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projects where

import GHC.Generics (Generic)

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity

import Smos.Data

import Smos.Report.Agenda.Types
import Smos.Report.Path
import Smos.Report.TimeBlock

data ProjectEntry = ProjectEntry
    { projectEntryFilePath :: RootedPath
    , projectEntryCurrentEntry :: Maybe Entry
    } deriving (Show, Eq, Generic)

instance Validity ProjectEntry

makeProjectEntry :: RootedPath -> SmosFile -> ProjectEntry
makeProjectEntry rp sf =
    ProjectEntry
        { projectEntryFilePath = rp
        , projectEntryCurrentEntry = getCurrentEntry sf
        }

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
