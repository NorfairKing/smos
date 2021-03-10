{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Projects
  ( smosQueryProjects,
  )
where

import Conduit
import Smos.Data
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Filter
import Smos.Report.Projects
import Smos.Report.Streaming

smosQueryProjects :: ProjectsSettings -> Q ()
smosQueryProjects ProjectsSettings {..} = do
  projs <-
    sourceToList $
      streamSmosProjectsQ
        .| smosMFilter (FilterFst <$> projectsSetFilter)
  cc <- asks smosQueryConfigColourConfig
  outputChunks $ renderProjectsReport cc $ makeProjectsReport projs

renderProjectsReport :: ColourConfig -> ProjectsReport -> [Chunk]
renderProjectsReport cc = formatAsBicolourTable cc . map renderProjectEntry . projectsReportEntries

renderProjectEntry :: ProjectEntry -> [Chunk]
renderProjectEntry ProjectEntry {..} =
  pathChunk projectEntryFilePath :
  case projectEntryCurrentEntry of
    Nothing -> [chunk "No next action"]
    Just e@Entry {..} -> [mTodoStateChunk $ entryState e, headerChunk entryHeader]
