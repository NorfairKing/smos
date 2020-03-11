{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projects where

import GHC.Generics (Generic)

import Control.Applicative
import Control.Monad
import Data.List
import Data.Validity

import Smos.Data

import Smos.Report.Path

newtype ProjectsReport =
  ProjectsReport
    { projectsReportEntries :: [ProjectEntry]
    }
  deriving (Show, Eq, Generic)

instance Validity ProjectsReport

makeProjectsReport :: [(RootedPath, SmosFile)] -> ProjectsReport
makeProjectsReport =
  ProjectsReport .
  sortOn (fmap entryState . projectEntryCurrentEntry) . map (uncurry makeProjectEntry)

data ProjectEntry =
  ProjectEntry
    { projectEntryFilePath :: RootedPath
    , projectEntryCurrentEntry :: Maybe Entry
    }
  deriving (Show, Eq, Generic)

instance Validity ProjectEntry

makeProjectEntry :: RootedPath -> SmosFile -> ProjectEntry
makeProjectEntry rp sf =
  ProjectEntry {projectEntryFilePath = rp, projectEntryCurrentEntry = getCurrentEntry sf}

getCurrentEntry :: SmosFile -> Maybe Entry
getCurrentEntry = goF . smosFileForest
  where
    goF :: Forest Entry -> Maybe Entry
    goF f = msum $ map goT $ reverse f
    goT :: Tree Entry -> Maybe Entry
    goT (Node e f) =
      case entryState e of
        Nothing -> Nothing
        Just ts ->
          if isDone ts
            then Nothing
            else goF f <|>
                 if isCurrent ts
                   then Just e
                   else Nothing
    isDone :: TodoState -> Bool
    isDone "DONE" = True
    isDone "CANCELLED" = True
    isDone "FAILED" = True
    isDone _ = False
    isCurrent :: TodoState -> Bool
    isCurrent "TODO" = False
    isCurrent _ = True
