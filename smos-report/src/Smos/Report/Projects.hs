{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Projects where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Data

newtype ProjectsReport = ProjectsReport
  { projectsReportEntries :: [ProjectEntry]
  }
  deriving (Show, Eq, Generic)

instance Validity ProjectsReport

makeProjectsReport :: [(Path Rel File, SmosFile)] -> ProjectsReport
makeProjectsReport =
  ProjectsReport
    . sortOn (fmap entryState . projectEntryCurrentEntry)
    . map (uncurry makeProjectEntry)

data ProjectEntry = ProjectEntry
  { projectEntryFilePath :: Path Rel File,
    projectEntryCurrentEntry :: Maybe Entry
  }
  deriving (Show, Eq, Generic)

instance Validity ProjectEntry

makeProjectEntry :: Path Rel File -> SmosFile -> ProjectEntry
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
          if todoStateIsDone ts
            then Nothing
            else
              goF f
                <|> if isCurrent ts
                  then Just e
                  else Nothing
    isCurrent :: TodoState -> Bool
    isCurrent "TODO" = False
    isCurrent _ = True
