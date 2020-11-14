{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Projects
  ( smosQueryProjects,
  )
where

import Conduit
import Rainbow
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
      streamSmosProjects
        .| streamParseSmosProjects
        .| smosMFilter (FilterFst <$> projectsSetFilter)
  liftIO $ putTableLn $ renderProjectsReport $ makeProjectsReport projs

renderProjectsReport :: ProjectsReport -> Table
renderProjectsReport = formatAsTable . map renderProjectEntry . projectsReportEntries

renderProjectEntry :: ProjectEntry -> [Chunk]
renderProjectEntry ProjectEntry {..} =
  pathChunk projectEntryFilePath
    : case projectEntryCurrentEntry of
      Nothing -> [chunk "No next action"]
      Just e@Entry {..} -> [mTodoStateChunk $ entryState e, headerChunk entryHeader]
