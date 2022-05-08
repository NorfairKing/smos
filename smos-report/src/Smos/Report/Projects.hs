{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projects where

import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Data

newtype ProjectsReport = ProjectsReport
  { projectsReportEntries :: [ProjectEntry]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ProjectsReport)

instance Validity ProjectsReport

instance HasCodec ProjectsReport where
  codec = dimapCodec ProjectsReport projectsReportEntries codec

makeProjectsReport :: [(Path Rel File, SmosFile)] -> ProjectsReport
makeProjectsReport =
  ProjectsReport
    . sortOn (fmap entryState . projectEntryCurrentEntry)
    . map (uncurry makeProjectEntry)

data ProjectEntry = ProjectEntry
  { projectEntryFilePath :: Path Rel File,
    projectEntryCurrentEntry :: Maybe Entry
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ProjectEntry)

instance Validity ProjectEntry

instance HasCodec ProjectEntry where
  codec =
    object "ProjectEntry" $
      ProjectEntry
        <$> requiredField "path" "project filepath, relative to the workflow directory" .= projectEntryFilePath
        <*> requiredField "current" "current entry" .= projectEntryCurrentEntry

makeProjectEntry :: Path Rel File -> SmosFile -> ProjectEntry
makeProjectEntry rp sf =
  ProjectEntry {projectEntryFilePath = rp, projectEntryCurrentEntry = getCurrentEntry sf}

getCurrentEntry :: SmosFile -> Maybe Entry
getCurrentEntry = goF . smosFileForest
  where
    goF :: Forest Entry -> Maybe Entry
    goF = msum . map goT
    goT :: Tree Entry -> Maybe Entry
    goT (Node e f) =
      if isCurrent (entryState e)
        then Just e
        else goF f
    isCurrent :: Maybe TodoState -> Bool
    isCurrent Nothing = False
    isCurrent (Just "TODO") = False
    isCurrent _ = True
